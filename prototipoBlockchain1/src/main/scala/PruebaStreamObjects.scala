import java.nio.ByteBuffer

import scala.util.Random

object PruebaStreamObjects extends App {
  val random = new Random()
  for(i <- 0 to 100){
    val l = random.nextLong()
    val encoded = ByteBuffer.allocate(8)
    LongerStreamObject.encodeImpl(l,0,encoded)
    val decoded = LongerStreamObject.decodeImpl(0,encoded).right.get.decoded
    if(!l.equals(decoded)){
      System.out.println("Original: " + l + " Decoded: " + decoded)
    }
  }

  for(i <- 0 to 100){
    val trSrc = new Array[Byte](32)
    random.nextBytes(trSrc)
    val trDst = new Array[Byte](32)
    random.nextBytes(trDst)
    val srcValidation = new Array[Byte](32)
    random.nextBytes(srcValidation)
    val tr = SimpleTransaction(trSrc,trDst,random.nextLong(),random.nextLong(),random.nextLong(),random.nextLong(),srcValidation)
    val encoded = ByteBuffer.allocate(SimpleTransactionCodecContract.size(tr))
    SimpleTransactionCodecContract.encodeImpl(tr,0,encoded)
    val dec = SimpleTransactionCodecContract.decodeImpl(0,encoded).right.get.decoded
    if(!tr.src.sameElements(dec.src)) System.out.println("DIFERENT trans.src")
    if(!tr.dst.sameElements(dec.dst)) System.out.println("DIFERENT trans.dst")
    if(!tr.srcValidation.sameElements(dec.srcValidation)) System.out.println("DIFERENT trans.srcValidation")
    if(!tr.srcRest.equals(dec.srcRest)) System.out.println("DIFERENT trans.srcRest")
    if(!tr.dstRest.equals(dec.dstRest)) System.out.println("DIFERENT trans.dstRest")
    if(!tr.mountToMiner.equals(dec.mountToMiner)) System.out.println("DIFERENT trans.mountToMiner")
    if(!tr.index.equals(dec.index)) System.out.println("DIFERENT trans.index")
  }

  val listIntCodecContract = new ListCodecContract[Int](IntegerCodecContract)
  for(i <- 0 to 100){
    var l:List[Int] = Nil
    for(j <- 0 to ((random.nextInt()) % 8) ){
      l = random.nextInt() :: l
    }
    val encoded = ByteBuffer.allocate(listIntCodecContract.size(l))
    listIntCodecContract.encodeImpl(l,0,encoded)
    val dec = listIntCodecContract.decodeImpl(0,encoded).right.get.decoded
    for(el <- l.zip(dec)){
      if(!el._1.equals(el._2)) System.out.println("DIFERENT ELEMENT")
    }
  }

  for(i <- 0 to 100){
    val prev_hash = new Array[Byte](32)
    random.nextBytes(prev_hash)
    val nonce = new Array[Byte](8)
    random.nextBytes(nonce)
    val dstRefund = new Array[Byte](32)
    random.nextBytes(dstRefund)
    val refTrans = RefundTransaction(dstRefund,random.nextLong())
    var listTransactions:List[SimpleTransaction] = Nil
    for(j <- 0 to (random.nextInt() % 8)){
      val trSrc = new Array[Byte](32)
      random.nextBytes(trSrc)
      val trDst = new Array[Byte](32)
      random.nextBytes(trDst)
      val srcValidation = new Array[Byte](32)
      random.nextBytes(srcValidation)
      listTransactions = SimpleTransaction(trSrc,trDst,random.nextLong(),random.nextLong(),random.nextLong(),random.nextLong(),srcValidation) :: listTransactions
    }
    val nod = NodeBlockChain(prev_hash,listTransactions,refTrans,nonce,random.nextLong())
    val encoded = ByteBuffer.allocate(Extras.maxNodeLength+4)
    NodeCodecContract.encodeImpl(nod,0,encoded)
    val decoded = NodeCodecContract.decodeImpl(0,encoded).right.get.decoded
    if(!nod.prev_hash.sameElements(decoded.prev_hash)) System.out.println("DIFERENT prev_has")
    if(!nod.index.equals(decoded.index)) System.out.println("DIFERENT index")
    if(!nod.refundTransaction.dst.sameElements(decoded.refundTransaction.dst)) System.out.println("DIFERENT ref.Dst")
    if(!nod.refundTransaction.dstRest.equals(decoded.refundTransaction.dstRest)) System.out.println("DIFERENT ref.Dst")
    if(!nod.transactions.size.equals(decoded.transactions.size)) System.out.println("DIFERENTE TAMAÑO transactions")
    for(trans <- nod.transactions.zip(decoded.transactions)){
      if(!trans._1.src.sameElements(trans._2.src)) System.out.println("DIFERENT trans.src")
      if(!trans._1.dst.sameElements(trans._2.dst)) System.out.println("DIFERENT trans.dst")
      if(!trans._1.srcValidation.sameElements(trans._2.srcValidation)) System.out.println("DIFERENT trans.srcValidation")
      if(!trans._1.srcRest.equals(trans._2.srcRest)) System.out.println("DIFERENT trans.srcRest")
      if(!trans._1.dstRest.equals(trans._2.dstRest)) System.out.println("DIFERENT trans.dstRest")
      if(!trans._1.mountToMiner.equals(trans._2.mountToMiner)) System.out.println("DIFERENT trans.mountToMiner")
      if(!trans._1.index.equals(trans._2.index)) System.out.println("DIFERENT trans.index")
    }
    if(!nod.hashNode.sameElements(decoded.hashNode)) System.out.println("DIFERENT hashNode")
  }
}
