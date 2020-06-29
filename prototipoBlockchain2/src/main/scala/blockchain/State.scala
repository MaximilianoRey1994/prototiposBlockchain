package blockchain
import java.security.{MessageDigest, SecureRandom}

import librarys.codec.BlockCodec
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.collection.mutable
import librarys.crypto

class State(maxDeepMemory:Int,folder:String,genesis:Block) {
  val random = new SecureRandom()
  var minNodeIndex:Long = 0
  var lastInDisk:Long = 0
  var maxLength:Long = 0
  var lastBlock:Block = genesis
  val blocks = new mutable.HashMap[BitVector,Block]()
  val sons = new mutable.HashMap[BitVector,List[BitVector]]()
  val freeUtxosDisk = new mutable.HashMap[BitVector,UTXO]()
  val diskMap = new DiskMap(folder)
  val pendingTransactions = new mutable.HashSet[Transaction]()

  blocks.put(BitVector(genesis.hash),genesis)
  sons.put(BitVector(genesis.hash),Nil)

  def purgeBranch(b:BitVector):Unit = {
    val s = sons(b)
    blocks.remove(b)
    sons.remove(b)
    for(son <- s){
      purgeBranch(son)
    }
  }
  def saveToDisk(block: Block):Unit = {
    diskMap.set(lastInDisk,block)
    lastInDisk = lastInDisk + 1
    for(t <- block.transaction){
      for(inp <- t.inputs){
        freeUtxosDisk.remove(inp.utxo.publicKey)
      }
      for(out <- t.outputs){
        freeUtxosDisk.+=((out.publicKey,out))
      }
    }
  }

  def encontrarPuntoEnComun(nuevo:Block,anterior:Block):List[Block] = {
    if(nuevo.hash.equals(anterior.hash)) List(nuevo)
    else (nuevo :: (encontrarPuntoEnComun(blocks(BitVector(nuevo.prevHash)),blocks(BitVector(anterior.prevHash)))))
  }

  def addBlock(block:Block):Boolean = {
    blocks.+=((BitVector(block.hash),block))
    sons.+=((BitVector(block.hash),Nil))
    val nuevoMaximo = block.index>maxLength

    if(nuevoMaximo){
      lastBlock = block
      maxLength = block.index
    }
    if(block.index - minNodeIndex > maxDeepMemory){
      var anterior:Block = null
      var actual = block
      while(actual.index!=minNodeIndex){
        anterior = actual
        actual = blocks(BitVector(actual.prevHash))
      }
      for(b <- sons(BitVector(actual.hash))){
        if(b != BitVector(anterior.hash)){
          purgeBranch(b)
        }
      }
      saveToDisk(actual)
      blocks.remove(BitVector(actual.hash))
      sons.remove(BitVector(actual.hash))
      minNodeIndex = minNodeIndex + 1
    }
    nuevoMaximo
  }

  @tailrec
  private def getBlockForIndex(i:Long,source:Block):Block = {
    if(i<0) throw new RuntimeException("PROBLEM, INDEX NEGATIVE")
      if(i<minNodeIndex){
        diskMap.get(i)
      }else if(i==source.index){
        source
      }else {
        getBlockForIndex(i,blocks(BitVector(source.prevHash)))
      }
  }
  private def verifyProofOfWork(b:Block):Boolean = {
    val oldBlock = getBlockForIndex(Utils.firstPartHash(b.prevHash).abs % (b.index),blocks(BitVector(b.prevHash)));
    val digest = MessageDigest.getInstance("SHA-256");
    digest.update(BlockCodec.encode(oldBlock).require.toByteArray ++ b.prevHash.slice(8,16))
    val target = digest.digest()
    BitVector(b.hash).take(21).equals(BitVector(target).take(21))
  }

  private def verifyUTXOInTransaction(t:Transaction,u:UTXO):Int = {
    val out = t.outputs.foldRight(1)((x,rec) => {
      if(x.publicKey.equals(u.publicKey)){
        if(x.equals(u))2
        else 3
      }else rec
    })
    if(out!=1)out
    else{
      t.inputs.foldRight(1)((x,rec) => {
        if(x.utxo.publicKey.equals(u.publicKey))0
        else rec
      })
    }
  }

  @tailrec
  private def verifyUTXOFromBlock(b:Block,u:UTXO):Int = {
    val t = b.transaction.foldRight(1)((x,rec) => {
      val verForTr = verifyUTXOInTransaction(x,u)
      if(verForTr==1) rec
      else verForTr
    })
    if(t==0) 0
    else if(t==2) 2
    else if(t==3) 3
    else if(u.publicKey.equals(b.minerUTXO.publicKey)){
      if(u.equals(b.minerUTXO)) 2
      else 3
    } else if(b.index==minNodeIndex){
      val otherUTXO = freeUtxosDisk.get(u.publicKey)
      if(otherUTXO.isEmpty) 0
      else if(otherUTXO.get.equals(u)) 2
      else 3
    }else{
      verifyUTXOFromBlock(blocks(BitVector(b.prevHash)),u)
    }
  }

  def verifyTransaction(b:Block,t:Transaction):Boolean = {
    t.inputs.foldRight(true)((x,rec) => {
      if(verifyUTXOFromBlock(b,x.utxo)==2) rec
      else false
    }) && t.outputs.foldRight(true)((x,rec) => {
      if(verifyUTXOFromBlock(b,x)==0) rec
      else false
    })
  }

  def verifyTransactionOfBlock(b:Block):Boolean = {
    b.transaction.foldRight(true)((t,rec) => {
      if(!t.verify()) false
      else if(!verifyTransaction(blocks(BitVector(b.prevHash)),t)) false
      else rec
    })
  }

  def verifyUTXOMiner(b:Block):Boolean = {
    var balance:Long = 0
    b.transaction.foldRight(())((x,rec) => {
      balance = balance + x.inputs.foldRight(0:Long)((u,r) => u.utxo.value + r)
      balance = balance - x.outputs.foldRight(0:Long)((u,r) => u.value + r)
      rec
    })
    if(balance < 0) false
    else b.minerUTXO.value.equals(5 + balance)
  }

  def verifyAndAddBlock(b:Block): Int ={
    if(!blocks.isDefinedAt(BitVector(b.prevHash))) 0
    else if(!verifyProofOfWork(b)) 0
    else if(!verifyTransactionOfBlock(b)) 0
    else if(!verifyUTXOMiner(b)) 0
    else if(addBlock(b)) {System.out.println("NEW HEAD");2}
    else {System.out.println("NEW BLOCK");1}
  }

  def verifyAndAddBlockWithhoutPOF(b:Block): Int ={
    if(!blocks.isDefinedAt(BitVector(b.prevHash))) 4
    else if(!verifyTransactionOfBlock(b)) 5
    else if(!verifyUTXOMiner(b)) 6
    else if(addBlock(b)) 2
    else 1
  }

  def verifyAndAddTransaction(t:Transaction): Boolean = {
    val res = t.verify()
    if(res) pendingTransactions.add(t)
    res
  }

  def genUTXO(transactions: List[Transaction]): UTXO ={
    var balance:Long = 0
    transactions.foldRight(())((x,rec) => {
      balance = balance - x.inputs.foldRight(0:Long)((u,r) => u.utxo.value + r)
      balance = balance + x.outputs.foldRight(0:Long)((u,r) => u.value + r)
      rec
    })
    val keyPair = crypto.generateKeyPair(random)
    UTXO(BitVector(crypto.encodeKey(keyPair.getPublic)),balance + 5)
  }

  def newBlock():(Block,Array[Byte]) = {
    @tailrec
    def aux(t:List[Transaction],res:List[Transaction]):List[Transaction] = {
      if(res.length>10) res
      else t match{
        case Nil => res
        case x :: ls => {
          if(verifyTransaction(lastBlock,x)) aux(ls,x::res)
          else aux(ls,res)
        }
      }
    }
    val transactions = aux(pendingTransactions.toList,Nil)
    val oldBlock = getBlockForIndex(Utils.firstPartHash(lastBlock.hash).abs % (maxLength + 1) ,lastBlock);
    val digest = MessageDigest.getInstance("SHA-256");
    digest.update(BlockCodec.encode(oldBlock).require.toByteArray ++ lastBlock.hash.slice(8,16))
    val targetHash = digest.digest()
    transactions.foldRight(())((x,rec) => {pendingTransactions.remove(x);rec})
    (Block(lastBlock.hash,transactions,Array.fill[Byte](8)(0),maxLength+1,genUTXO(transactions)),targetHash)
  }

  def getBlock(i:Long): Block ={
    this.getBlockForIndex(i,lastBlock)
  }
}
