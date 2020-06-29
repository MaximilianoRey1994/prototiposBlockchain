import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

import scala.util.Random
import java.security.{KeyPairGenerator, PrivateKey, PublicKey}
import java.util.concurrent.ForkJoinTask

import scodec.bits.BitVector

import scala.annotation.tailrec

class BlockChain(actual:NodeBlockChain,anteriores:BlockChain){
  def this(node:NodeBlockChain) = this(node,null)
  def addNode(nuevo:NodeBlockChain): BlockChain ={
    if (acceptNode(nuevo)) new BlockChain(nuevo,this)
    else this
  }
  def acceptNode(nuevo:NodeBlockChain): Boolean = nuevo.prev_hash.sameElements(actual.hashNode) && Condition.hashGoodEnought(nuevo.hashNode)
}


class Miner(publicKey: PublicKey,privateKey: PrivateKey,addr:InetSocketAddress,state:State,miningEnabled:Boolean,myID:BitVector,head: Option[(BitVector,InetSocketAddress)],id:Int = 0) extends Actor{
  Logging(context.system, self)
  val actualRandom = new Random()

  val encodedPublicKey = if(publicKey.getEncoded.length<32) publicKey.getEncoded ++ Array.fill[Byte](32 - publicKey.getEncoded.length)(0) else publicKey.getEncoded

  //val hashCalculator:HashCalculator = new HashCalculator(actualRandom,self)
  val proxy = context.actorOf(Props(new Proxy(myID,addr,head,self)))
  var calculingHash: ForkJoinTask[Unit] = null
  if (miningEnabled){calculingHash = Extras.task(calculateHash(constructIncompleteNode(state.nodes.get(state.listHash.last).get)))}

  override def receive(): Actor.Receive = {
    case ReceivedNode (n) => {System.out.println(id + ": RECIBIDO: " + n.nonce.toSeq + " " + n.hashNode(0) + " " + n.hashNode(1) + " " + n.hashNode(2));val resN = state.receiveNode(n); if(resN>0) {System.out.println(id +": RECIBIDO VALIDO");proxy ! ReceivedNode(n)} else System.out.println(id + ": RECIBIDO INVALIDO");if(resN>1){synchronized(if(calculingHash!=null) calculingHash.cancel(true));if (miningEnabled){calculingHash = Extras.task(calculateHash(constructIncompleteNode(n)))}}}
    case NewTransaction(t) => {if(state.addTransaction(t)) proxy ! NewTransaction(t)}
    case ProcessedNode(n) => {System.out.println(id + ": GENERADO: " + n.nonce.toSeq + " " + n.hashNode(0) + " " + n.hashNode(1)+ " " + n.hashNode(2));val resN = state.receiveNode(n);if(resN>0){System.out.println(id + ": VALIDO GENERADO"); proxy ! ReceivedNode(n)}; if(resN>1){calculingHash = Extras.task(calculateHash(constructIncompleteNode(n)))}}
    case Quest(q) => {
      val nod = getNode(q)
      if(nod!=null){
        proxy ! ReceivedNode(nod)
      }
    }
    case _ => System.err.println("ERROR")
      null
  }
  def constructIncompleteNode(n:NodeBlockChain): NodeBlockChain ={
    val valid:(List[SimpleTransaction],Long) = state.takeValidsTransactions(n)
    val newFounds = state.foundOf(encodedPublicKey) + valid._2
    System.out.println("New founds: " + newFounds)
    NodeBlockChain(n.hashNode,valid._1,new RefundTransaction(encodedPublicKey,newFounds),Array(0,0,0,0,0,0,0,0),n.index+1)
  }
  def getNode(index:Long): NodeBlockChain ={
    if (state.nodes.get(state.listHash.last).get.index < index) null
    else{
      val firstNodeIndex = state.nodes.get(state.listHash.head).get.index
      if(firstNodeIndex > index){
        state.diskNodes.readNode(index)
      }else state.nodes(state.listHash((index - firstNodeIndex).toInt))
    }
  }


  def calculateHash(incompleteNode: NodeBlockChain) {
    @tailrec
    def calculateHashAux(): Unit = {
      val newNonce: Array[Byte] = Extras.longToByte(actualRandom.nextLong())
      val newNode = NodeBlockChain(incompleteNode.prev_hash, incompleteNode.transactions, incompleteNode.refundTransaction, newNonce, incompleteNode.index)
      if (Condition.hashGoodEnought(newNode.hashNode)) {self ! ProcessedNode(newNode) }
      else calculateHashAux()
    }
    calculateHashAux()
  }
}

object MainObjectMiner{
  def main(argv:Array[String]): Unit ={
    val system = ActorSystem("iot-system")

    val keyGen = KeyPairGenerator.getInstance("RSA")
    val pairKeys1 = keyGen.generateKeyPair
    val privateKey1 = pairKeys1.getPrivate
    val publicKey1 = pairKeys1.getPublic
    val addr1 = new InetSocketAddress(InetAddress.getByName("127.0.0.1"),5555)
    val state1 = new State("genesis",true)

    val system2 = ActorSystem("iot-system")
    val pairKeys2 = keyGen.generateKeyPair
    val privateKey2 = pairKeys2.getPrivate
    val publicKey2 = pairKeys2.getPublic
    val addr2 = new InetSocketAddress(InetAddress.getByName("127.0.0.2"),5556)
    val state2 = new State("genesis",true)

    val system3 = ActorSystem("iot-system")
    val pairKeys3 = keyGen.generateKeyPair
    val privateKey3 = pairKeys3.getPrivate
    val publicKey3 = pairKeys3.getPublic
    val addr3 = new InetSocketAddress(InetAddress.getByName("127.0.0.3"),5557)
    val state3 = new State("genesis",true)

    val random = new Random()
    val id1 = {val arr = new Array[Byte](20);random.nextBytes(arr);BitVector(arr)}
    val id2 = {val arr = new Array[Byte](20);random.nextBytes(arr);BitVector(arr)}
    val id3 = {val arr = new Array[Byte](20);random.nextBytes(arr);BitVector(arr)}
    val rootAddr = new InetSocketAddress("127.0.0.1",5555)
    Extras.parallel(Extras.parallel(system.actorOf(Props(new Miner(publicKey1,privateKey1,addr1,state1,true,id1,None))),
    system2.actorOf(Props(new Miner(publicKey2,privateKey2,addr2,state2,true,id2,Some((id1,addr1)),1)))),
    system3.actorOf(Props(new Miner(publicKey3,privateKey3,addr3,state3,true,id3,Some((id1,addr1)),2))))
    //system.actorOf(Props(new Miner(publicKey1,privateKey1,addr1,state1,true,null)))
  }
}