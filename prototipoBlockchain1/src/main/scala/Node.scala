import java.security.MessageDigest

import scala.annotation.tailrec
import scala.util.Random

case class ProcessNode(n:NodeBlockChain,m:Array[Byte])
case class ProcessedNode(n:NodeBlockChain)
case class ReceivedNode (n:NodeBlockChain)

case class SimpleTransaction(src:Array[Byte],dst:Array[Byte],val mountToMiner:Long,srcRest:Long,dstRest:Long,index:Long,srcValidation:Array[Byte]) extends Transaction {
  override def length: Long = 32 + 32 + 8 +8 + 8 + 8 + 32
  override lazy val getBytes: Array[Byte] =  src ++ dst ++ Extras.longToByte(mountToMiner)  ++ Extras.longToByte(srcRest) ++ Extras.longToByte(dstRest) ++ Extras.longToByte(index) ++ srcValidation
  lazy val hashWithoutValidation = MessageDigest.getInstance("SHA-256").digest(src ++ dst ++ Extras.longToByte(mountToMiner)  ++ Extras.longToByte(srcRest) ++ Extras.longToByte(dstRest)++ Extras.longToByte(index))
  override def toString:String = "src: " ++ src.toString ++ " dst: " ++ dst.toString ++ " mount to miner: " ++ mountToMiner.toString ++ " srcRest: " ++ srcRest.toString ++ " dstRest " ++ dstRest.toString ++ " srcValidation: " ++ srcValidation.toString
  override def newStates:List[(Array[Byte],Long)] = (src,srcRest) :: (dst,dstRest) :: Nil
}

case class RefundTransaction(dst:Array[Byte],dstRest:Long) extends Transaction {
  override def length: Long = 8 + 32
  val  mountToMiner:Long = 0

  override lazy val getBytes: Array[Byte] = dst ++ Extras.longToByte(dstRest)
  override def newStates:List[(Array[Byte],Long)] = (dst,dstRest) :: Nil

  override def toString: String = {
    "[dst: " + dst.toSeq + " dstRest: " + dstRest + "]"
  }
}

object Condition{
  def hashGoodEnought(hashNode:Array[Byte]): Boolean ={
    //(hashNode(0) & 0xC0).equals(0)
    (hashNode(0)).equals(0.toByte) && (hashNode(1)).equals(0.toByte) && (hashNode(2) & 0xF3).equals(0)
    //true
  }
}

case class NodeBlockChain(prev_hash:Array[Byte], transactions:List[SimpleTransaction], refundTransaction: RefundTransaction, nonce:Array[Byte], index:Long){
  val hashNode:Array[Byte] = {
    val arr = prev_hash ++ transactions.foldRight(Array():Array[Byte])((x, rec) => x.getBytes ++ rec) ++ refundTransaction.getBytes ++ nonce ++ Extras.longToByte(index)
    val d = MessageDigest.getInstance("SHA-256")
    d.digest(arr)
  }

  def arrayToString(arr: Array[Byte]): String = {
    "[" ++ arr.foldRight("]")((x,rec) => "0x" ++ x.toHexString ++ "," ++rec)
    //"[" ++ arr.mkString("+") + "]"
  }

  val getBytesAux:Array[Byte] = prev_hash ++ transactions.foldRight(Array():Array[Byte])((x, rec) => x.getBytes ++ rec) ++ refundTransaction.getBytes ++ Extras.longToByte(index) ++ nonce
  val getBytes =  Extras.intToByte(transactions.size) ++ getBytesAux



  override def toString: String ={
    "prev hash: " + arrayToString(prev_hash) + " mensage: " + transactions.toString() + " refund transaction: " + refundTransaction + " nonce: " + arrayToString(nonce) +  " actual hash: " + arrayToString(hashNode)
  }

}