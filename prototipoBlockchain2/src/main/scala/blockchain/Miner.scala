package blockchain

import akka.actor.ActorRef
import scodec.bits.BitVector

import scala.util.Random

class Miner(r:ActorRef) extends Runnable{
  var thread:Thread = null
  var continueMining = true
  var blockMining:Block = null
  var hashTarget:BitVector = null
  val random = new Random()
  def mineBlock(b:Block,h:BitVector):Unit = {
    if(thread!=null){
      continueMining = false
      thread.join()
    }
    continueMining = true
    blockMining = b
    hashTarget = h.take(21)
    thread = new Thread(this)
    thread.start()
  }

  override def run(): Unit = {
    while(continueMining){
      val nonce = new Array[Byte](8)
      random.nextBytes(nonce)
      val b = Block(blockMining.prevHash,blockMining.transaction,nonce,blockMining.index,blockMining.minerUTXO)
      if(BitVector(b.hash).take(21).equals(hashTarget)){
        r ! Utils.MiningBlock(b)
        continueMining = false
      }
    }
  }
}
