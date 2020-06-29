package blockchain

object Utils {
  def firstPartHash(hash:Array[Byte]):Long = {
    var res:Long = 0
    for(n <- hash.take(8)){
      res = (res << 8) + n
    }
    res
  }
  def secondPartHash(hash:Array[Byte]):Long = {
    var res:Long = 0
    for(n <- hash.slice(8, 16)){
      res = (res << 8) + n
    }
    res
  }
  case class MiningBlock(b:Block)
  case class TransactionReceived(t:Transaction)
  case class BlockReceived(b:Block)
}
