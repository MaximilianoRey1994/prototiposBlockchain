import java.io.{BufferedReader, FileReader}
import java.net.InetAddress

import io.iohk.decco.BufferInstantiator.global.HeapByteBuffer
import monix.execution.Scheduler.Implicits.global
import java.net.InetSocketAddress

import io.iohk.scalanet.peergroup._
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import io.iohk.scalanet.peergroup.kademlia.{KNetworkBeta, KRouter, KRouterBeta}
import scodec.bits.BitVector

class Proxy(myID:BitVector,myAddr:InetSocketAddress,head: Option[(BitVector,InetSocketAddress)],miner: ActorRef) extends Actor {
  Logging(context.system, self)

  var chanelsNodes:List[Channel[InetMultiAddress,Either[Either[NodeBlockChain,String],Either[SimpleTransaction,Quest]]]] = Nil
  var knowIPs:Set[String] = Set()

  val knownPeers:Set[KRouter.NodeRecord[InetMultiAddress]] = head match{
    case None => Set()
    case Some((id,addr)) => Set(KRouter.NodeRecord(id,InetMultiAddress(addr),InetMultiAddress(addr)))
  }

  val krouterConfig = KRouter.Config(KRouter.NodeRecord(myID,InetMultiAddress(myAddr),InetMultiAddress(myAddr)),knownPeers)
  val config = TCPPeerGroup.Config(myAddr)
  val network = KNetworkBeta.createKNetworkBetaTCP[Either[NodeBlockChain,SimpleTransaction]](config)(new EitherCodecContract[NodeBlockChain,SimpleTransaction](NodeCodecContract,SimpleTransactionCodecContract),HeapByteBuffer,global).runSyncUnsafe()
  val krouter = KRouterBeta.startRouterWithServerSeq(krouterConfig,network,sendToMiner).runSyncUnsafe()

  override def receive: Receive = {
    case ReceivedNode(n) => {System.out.println("SENDING NODE COMPLETE: " + n.toString); krouter.broadCastMessage(Left(n)).runAsyncAndForget}
    case NewTransaction(t) => {krouter.broadCastMessage(Right(t)).runAsyncAndForget}
  }

  private def sendToMiner(incoming:Either[NodeBlockChain,SimpleTransaction]):Unit = incoming match{
    case Left(n) => {System.out.println("RECEIVED NODE COMPLETE: " + n.toString);miner ! ReceivedNode(n)}
    case Right(t) => miner ! NewTransaction(t)
  }


}