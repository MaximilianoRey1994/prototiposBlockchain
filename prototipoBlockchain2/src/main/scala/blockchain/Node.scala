package blockchain

import java.net.{InetAddress, InetSocketAddress}
import java.security.SecureRandom

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging
import scodec.bits.BitVector
import librarys.crypto
import wallet.Wallet

class Node(id:Int,state: State,genesis:Block, myAddr:InetSocketAddress, onlyTransactionAddr:InetSocketAddress, nowIps:List[InetSocketAddress])  extends Actor{
  Logging(context.system, self)

  private val proxy = context.actorOf(Props(new Proxy(myAddr,onlyTransactionAddr,nowIps,state,self)))

  val miner = new Miner(self)

  private val newBlock = state.newBlock()
  miner.mineBlock(newBlock._1,BitVector(newBlock._2))

  override def receive: Receive = {
    case Utils.BlockReceived(x) => {
      if(state.verifyAndAddBlock(x)==2){
        System.out.println("VALID NODE RECEIVED: " + id + " index: " + x.index)
        val newBlock = state.newBlock()
        miner.mineBlock(newBlock._1,BitVector(newBlock._2))
      }else{
        System.out.println("INVALID NODE RECEIVED: " + id + " index: " + x.index)
      }
    }
    case Utils.TransactionReceived(x) => state.verifyAndAddTransaction(x)
    case Utils.MiningBlock(b) => {
      proxy ! Utils.BlockReceived(b)
      System.out.println("VALID NODE MINED: " + id + " index: " + b.index)
      if(state.verifyAndAddBlock(b)==2){
        val newBlock = state.newBlock()
        miner.mineBlock(newBlock._1,BitVector(newBlock._2))
      }
    }
    case _ => throw new RuntimeException("RECEIBED INVALID OBJECT")
  }
}

object Node extends App{
  val random = new SecureRandom()
  val system = ActorSystem("iot-system")


  val keyPair = crypto.generateKeyPair(random)
  val minerUTXO = UTXO(BitVector(crypto.encodeKey(keyPair.getPublic)),100000)
  val genesis = Block(Array.fill[Byte](32)(1), List(),Array.fill[Byte](8)(0),0,minerUTXO)

  val id1 = {val arr = new Array[Byte](20);random.nextBytes(arr);BitVector(arr)}
  val ip1 = new InetSocketAddress(InetAddress.getByName("127.0.0.1"),5555)
  val tIp1 = new InetSocketAddress(InetAddress.getByName("127.0.0.3"),5557)


  val state1 = new State(10,"./diskMap1",genesis)


  val system2 = ActorSystem("iot-system")
  val ip2 = new InetSocketAddress(InetAddress.getByName("127.0.0.2"),5556)
  val tIp2 = new InetSocketAddress(InetAddress.getByName("127.0.0.4"),5558)
  val state2 = new State(10,"./diskMap2",genesis)

  val system3 = ActorSystem("iot-system")
  val ip3 = new InetSocketAddress(InetAddress.getByName("127.0.0.5"),5559)
  val tIp3 = new InetSocketAddress(InetAddress.getByName("127.0.0.6"),5560)
  val state3 = new State(10,"./diskMap3",genesis)


  system.actorOf(Props(new Node(1,state1,genesis,ip1,tIp1,Nil)))
  Thread.sleep(1000)
  system2.actorOf(Props(new Node(2,state2,genesis,ip2,tIp2,List(ip1))))
  system3.actorOf(Props(new Node(3,state3,genesis,ip3,tIp3,List(ip1))))

  val wIp = new InetSocketAddress(InetAddress.getByName("127.0.0.7"),5561)
  val wIp2 = new InetSocketAddress(InetAddress.getByName("127.0.0.8"),5562)

  Thread.sleep(10000)
  val keyPair2 = crypto.generateKeyPair(random)
  val wallet = new Wallet(tIp1,wIp,List((minerUTXO,keyPair)))
  val wallet2 = new Wallet(tIp1,wIp2,Nil)
  wallet.sendToWallet(wIp2,10,3)

}