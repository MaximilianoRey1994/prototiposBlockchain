import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, PublicKey}

import akka.actor.ActorRef

import scala.annotation.tailrec
import scala.collection.mutable

case class ArrayDecorator(value:Array[Byte]){
  override def canEqual(that: Any): Boolean = that.isInstanceOf[ArrayDecorator]
  override def equals(o: Any): Boolean = {
    if(!o.isInstanceOf[ArrayDecorator]) false
    else{
      val o_cast = o.asInstanceOf[ArrayDecorator]
      o_cast.value.sameElements(this.value)
    }
  }

  override def hashCode(): Int = {
    this.value.sum
  }

  override def toString: String = value.toList.toString()
}

class State(files:String,readFile:Boolean) {

  def validFirm(t:SimpleTransaction): Boolean ={
    val publicKey:PublicKey  = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(t.src))
    Extras.decrypt(publicKey,t.srcValidation).sameElements(t.hashWithoutValidation)
  }
  def addTransaction(t: SimpleTransaction): Boolean = {
    val res = validFirm(t) && !transactionsPending.contains(t) && t.index >= transactionNumber.getOrElse(ArrayDecorator(t.src),0:Long) && t.mountToMiner>=0
    if(res) transactionsPending.add(t)
    res
  }

  val nodes:mutable.HashMap[ArrayDecorator,NodeBlockChain] = new mutable.HashMap[ArrayDecorator,NodeBlockChain]
  val sons = new mutable.HashMap[ArrayDecorator,List[ArrayDecorator]]
  val founds = new mutable.HashMap[ArrayDecorator,Long]()
  val listHash:mutable.ListBuffer[ArrayDecorator] = new mutable.ListBuffer[ArrayDecorator]
  val maxLengthList = 15
  val transactionNumber = new mutable.HashMap[ArrayDecorator,Long]()
  val transactionsPending = new mutable.HashSet[SimpleTransaction]()

  val diskNodes:DiskNodes = new DiskNodes(files + "DiskNode")

  def saveToDisk(fileNodes:String,fileSons:String,fileFounds:String,fileListHash:String,fileTransactionNumber:String): Unit ={
    {
      val f:FileOutputStream = new FileOutputStream(new File(fileNodes))
      for(current <- nodes){
        val arr = ByteBuffer.allocate(Extras.maxNodeLength+4)
       // System.out.println(current._1.value.toList)
        NodeCodecContract.encodeImpl(current._2,0,arr)
        f.write(current._1.value)
        f.write(arr.array())
      }
      f.close()
    }
    {
      val f:FileOutputStream = new FileOutputStream(new File(fileSons))
      val it = sons.iterator
      while(it.hasNext){
        val act = it.next()
        val enc = new ListCodecContract[Array[Byte]](Byte32CodecContract)
        f.write(act._1.value)
        val v_buff = ByteBuffer.allocate(enc.size(act._2.map(x => x.value)))
        enc.encodeImpl(act._2.map(x => x.value),0,v_buff)
        f.write(v_buff.array())
      }
      f.close()
    }
    {
      val f:FileOutputStream = new FileOutputStream(new File(fileFounds))
      val it = founds.iterator
      while(it.hasNext){
        val current = it.next()
        f.write(current._1.value ++ Extras.longToByte(current._2))
      }
      f.close()
    }
    System.out.println();
    {
      val f:FileOutputStream = new FileOutputStream(new File(fileListHash))
      for(elem <- listHash){
        //System.out.println(elem.value.toList)
        f.write(elem.value)
      }
      f.close()
    }
    {
      val f:FileOutputStream = new FileOutputStream(new File(fileTransactionNumber))
      val it = founds.iterator
      while(it.hasNext){
        val current = it.next()
        f.write(current._1.value ++ Extras.longToByte(current._2))
      }
      f.close()
    }
  }

  def close():Unit = {
    diskNodes.close()
    saveToDisk(files + "Nodes",files+ "Sons" , files +"Founds",files + "ListHash", files + "TransactionNumbers" )
  }

  def readFromDisk(fileNodes:String,fileSons:String,fileFounds:String,fileListHash:String,fileTransactionNumber:String): Unit ={
    {
      val f:FileInputStream = new FileInputStream(new File(fileNodes))
      @tailrec
      def readNodes(): Unit ={
        if(f.available()>0) {
          val key = new Array[Byte](32)
          val n = new Array[Byte](Extras.maxNodeLength + 4)
          f.read(key)
          System.out.println(key.toList)
          f.read(n)
          val dec = NodeCodecContract.decodeImpl(0, ByteBuffer.wrap(n))
          dec match {
            case Left(_) => throw new RuntimeException("Error loading")
            case Right(d) => {
              //System.out.println(d.decoded)
              nodes.put(ArrayDecorator(key), d.decoded)
              readNodes()
            }
          }
        }
      }
      readNodes()
      f.close()
    }
    {
      val f:FileInputStream = new FileInputStream(new File(fileSons))
      val sons_arr = new Array[Byte](f.available())
      f.read(sons_arr)
      val sons_arrbuff = ByteBuffer.wrap(sons_arr)
      val enc = new ListCodecContract[Array[Byte]](Byte32CodecContract)
      @tailrec
      def aux(i:Int): Unit ={
        if(i>sons_arr.length) throw new RuntimeException("Problem decode fileSons")
        else if(i<sons_arr.length){
          Byte32CodecContract.decodeImpl(i,sons_arrbuff) match {
            case Left(_) => new RuntimeException("Problem decode fileSons")
            case Right(dec1) => {
              enc.decodeImpl(dec1.nextIndex,sons_arrbuff) match{
                case Left(_) => new RuntimeException("Problem decode fileSons")
                case Right(dec2) => {
                  sons.put(ArrayDecorator(dec1.decoded),dec2.decoded.map(ArrayDecorator))
                  aux(dec2.nextIndex)
                }
              }
            }
          }
        }
      }
      aux(0)
      f.close()
    }
    {
      val f:FileInputStream = new FileInputStream(new File(fileFounds))
      while(f.available()>0){
        val key = new Array[Byte](32)
        val v = new Array[Byte](8)
        f.read(key)
        f.read(v)
        founds.put(ArrayDecorator(key),Extras.bytesToLong(v))
      }
      f.close()
    }
    System.out.println();
    {
      val f:FileInputStream = new FileInputStream(new File(fileListHash))
      while(f.available()>0){
        val v = new Array[Byte](32)
        f.read(v)
        listHash += ArrayDecorator(v)
        System.out.println(v.toList)
      }
      f.close()
    }
    {
      val f:FileInputStream = new FileInputStream(new File(fileTransactionNumber))
      while(f.available()>0){
        val key = new Array[Byte](32)
        val v = new Array[Byte](8)
        f.read(key)
        f.read(v)
        transactionNumber.put(ArrayDecorator(key),Extras.bytesToLong(v))
      }
      f.close()
    }
  }
  if(readFile) readFromDisk(files + "Nodes",files+ "Sons" , files +"Founds",files + "ListHash", files + "TransactionNumbers" )

  def transactionIndexOf(id:Array[Byte],n:NodeBlockChain): Long ={
    @tailrec
    def transactionIndexOfAux(n:NodeBlockChain):Long =
      if(n.hashNode.sameElements(listHash.head.value)) transactionNumber.getOrElse(ArrayDecorator(id),0)
      else{
        val t = n.transactions.filter(x => x.src.sameElements(id))
        if (!t.isEmpty) t.head.index
        else transactionIndexOfAux(nodes.get(ArrayDecorator(n.prev_hash)).get)
      }
      transactionIndexOfAux(n)
  }

  def foundsOf(id:Array[Byte],n:NodeBlockChain): Long ={
    if(n.hashNode.sameElements (listHash.head.value)) founds.getOrElse(ArrayDecorator(id), 0)
    else
    {
      if(n.refundTransaction.dst.sameElements(id)){
        n.refundTransaction.dstRest
      }else {
        val t = n.transactions.find(x => x.newStates.find(y => y._1 .sameElements(id)).nonEmpty)
        if (t.isEmpty) foundsOf(id, nodes.get(ArrayDecorator(n.prev_hash)).getOrElse(throw new RuntimeException("Error foundOf: nodes: nodo inexistente")))
        else t.get.newStates.find(y => y._1 sameElements id).get._2
      }
    }
  }

  def foundOf(id:Array[Byte]): Long ={


    val lasNode = this.nodes.get(this.listHash.last).getOrElse(throw  new RuntimeException("Problema foundOf simple: nodes"))
    foundsOf(id,lasNode)
  }


  def addTransactionsToFounds(n:NodeBlockChain):Unit = {
    n.transactions.foldRight(())((x, rec) => {founds.put(ArrayDecorator(x.src),x.srcRest);founds.put(ArrayDecorator(x.dst),x.dstRest);transactionNumber.put(ArrayDecorator(x.src),x.index);rec})
    founds.put(ArrayDecorator(n.refundTransaction.dst),n.refundTransaction.dstRest)
  }

  def deleteNodeInsecure_2(h:Array[Byte],exept:Array[Byte]): Unit ={
    sons.get(ArrayDecorator(h)).getOrElse(throw new RuntimeException("Error deleteNodeInsecure_2: sons: inexistente")).foldRight(())((x,rec) => {if(!x.value.sameElements(exept)) deleteNodeInsecure(x.value);rec})
    sons.remove(ArrayDecorator(h))
    nodes.remove(ArrayDecorator(h))
  }

  def deleteNodeInsecure(h:Array[Byte]): Unit ={
    sons.get(ArrayDecorator(h)).getOrElse(throw new RuntimeException("Error deleteNodeInsecure: sons: inexistente")).foldRight(())((x,rec) => {deleteNodeInsecure(x.value);rec})
    sons.remove(ArrayDecorator(h))
    nodes.remove(ArrayDecorator(h))
  }

  def deleteNodeSecure(h:Array[Byte]): Unit ={
    lazy val p = nodes.get(ArrayDecorator(h)).get.prev_hash
    sons.put(ArrayDecorator(p),sons.get(ArrayDecorator(p)).getOrElse(throw new RuntimeException("Error deleteNodeSecure: sons: nodo inexistente")).filter(x => !x.value.sameElements(h)))
  }

  def addNewNode(n:NodeBlockChain): Boolean ={
    val oldList = sons.get(ArrayDecorator(n.prev_hash)).getOrElse(throw new RuntimeException("Error addNewNode_1: sons: nodo inexistente"))
    if(oldList.foldRight(false)((x,rec) => {if (n.hashNode.sameElements(x.value)) true else rec})) false
    else {
      sons.put(ArrayDecorator(n.prev_hash), ArrayDecorator(n.hashNode) ::sons.get(ArrayDecorator(n.prev_hash)).get)
      nodes.put(ArrayDecorator(n.hashNode), n)
      sons.put(ArrayDecorator(n.hashNode),List())
      n.transactions.foldRight(())((x, rec) => {
        transactionsPending.remove(x); rec
      })
      lazy val ultimoNodo = nodes.get(listHash.last).getOrElse(throw new RuntimeException("Error addNewNode_2: nodes: nodo inexistente")).index
      if (n.index > ultimoNodo) {
        if (n.prev_hash.sameElements(listHash.last.value)) {
          if (listHash.length >= maxLengthList) {
            deleteNodeInsecure_2(listHash.head.value, listHash(1).value)
            listHash.remove(0)
            val nodesHead = nodes.get(listHash.head).getOrElse(throw new RuntimeException("Error addNewNode_3: nodes: nodo inexistente"))
            addTransactionsToFounds(nodesHead)
            diskNodes.writeNode(nodesHead.index, nodesHead)
          }
          listHash += ArrayDecorator(n.hashNode)
        } else {
          makeNewList(n)
        }
        true
      } else false
    }
  }
  def makeNewList(n:NodeBlockChain): Unit ={
    listHash.clear()
    def makeNewListAux(actualN:Array[Byte]): Unit ={
      listHash.insert(0,ArrayDecorator(actualN))
      makeNewListAux(nodes.get(ArrayDecorator(actualN)).getOrElse(throw new RuntimeException("Error makeNewListAux_1: nodes: nodo inexistente")).hashNode)
    }
    makeNewListAux(n.hashNode)
    if(listHash.length>=maxLengthList) {
      deleteNodeInsecure_2(listHash.head.value,listHash(1).value)
      listHash.remove(0)
      addTransactionsToFounds(nodes.get(listHash.head).getOrElse(throw new RuntimeException("Error makeNewListAux_2: nodes: nodo inexistente")))
    }
  }

  def validTransaction(x:SimpleTransaction,n:NodeBlockChain): Boolean ={
    if(!validFirm(x)) false
    else {
      val f = foundsOf(x.src, n)
      if (f < x.srcRest + x.mountToMiner) false
      else {
        val diff = f - x.srcRest - x.mountToMiner
        x.dstRest.equals(foundsOf(x.dst, n) + diff)  && x.index.equals(this.transactionIndexOf(x.src, n) + 1)
      }
    }
  }

  def takeValidsTransactions(n:NodeBlockChain):(List[SimpleTransaction],Long) ={
    val it = transactionsPending.iterator
    var listTr:List[SimpleTransaction] = Nil
    var totalFound:Long = 5
    while(it.hasNext && listTr.size<Extras.maxTransactionInNode){
      val transaction = it.next()
      if(validTransaction(transaction,n)){
        listTr = transaction ::listTr
        totalFound = totalFound + transaction.mountToMiner
      }
    }
    (listTr,totalFound)
  }

  def validateTransactions(xs:List[SimpleTransaction],n:NodeBlockChain): Boolean = xs match {
    case Nil => true
    case x::xs =>{
      val res = Extras.parallel(validTransaction(x,n),validateTransactions(xs,n))
      res._1 && res._2
    }
  }

  def validateNewNode(n:NodeBlockChain): Boolean ={
    if(nodes.get(ArrayDecorator(n.hashNode)).isDefined) {return false}
    if(nodes.get(ArrayDecorator(n.prev_hash)).isEmpty) {return false}
    lazy val prev_node  = nodes.get(ArrayDecorator(n.prev_hash)).getOrElse(throw new RuntimeException("Error validateNewNode: nodes: nodo inexistente"))
    if(!Condition.hashGoodEnought(n.hashNode)) false
    else if(prev_node.index+1!=n.index) false
    else if(!validateTransactions(n.transactions,prev_node)) false
    else{
      val sumMountToMiners = n.transactions.map(x => x.mountToMiner).sum
      (sumMountToMiners+5).equals(n.refundTransaction.dstRest - foundsOf(n.refundTransaction.dst,prev_node))
    }
  }

  def receiveNode(n:NodeBlockChain): Int ={
    if(validateNewNode(n)) {
      if(addNewNode(n)) 2
      else 1
    }
    else 0
  }
}