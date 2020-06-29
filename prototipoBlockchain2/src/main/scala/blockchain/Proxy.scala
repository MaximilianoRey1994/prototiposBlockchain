package blockchain

import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import librarys.codec.{BlockCodec, SignedBroadcastMessageCodec, TransactionCodec}
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs.either
import scodec.codecs.implicits._
import librarys.crypto

import scala.collection.mutable

class Proxy(myAddr:InetSocketAddress, onlyTransactionAddr:InetSocketAddress, knowIps:List[InetSocketAddress], state: State, node: ActorRef) extends Actor with Runnable{
  Logging(context.system, self)
  val random = new SecureRandom()

  val keyPair = crypto.generateKeyPair(random)
  val myId = BitVector(crypto.encodeKey(keyPair.getPublic))

  implicit val codec:Codec[Either[Block,Either[Transaction,Long] ]] = either(implicitBooleanCodec,BlockCodec,either(implicitBooleanCodec,TransactionCodec,implicitLongCodec))

  val nextPerId = new mutable.HashMap[BitVector,Long]
  val nextMessage = new mutable.HashMap[BitVector,mutable.HashMap[Long,Either[Block,Either[Transaction,Long]]]]()

  var sockets: List[Socket] = knowIps.map(x => new Socket(x.getAddress,x.getPort))
  sockets.foldRight(())((x,rec) => {new Thread(new ListenSocket(x)).start();rec})

  class SocketTransactionListen(s:Socket) extends Runnable{
    val input = s.getInputStream
    override def run(): Unit = {
      while(!s.isClosed){
        val buffLength = new Array[Byte](4)
        input.read(buffLength)
        val messageLength = implicitIntCodec.decode(BitVector(buffLength)).require
        val buff = new Array[Byte](messageLength.value)
        input.read(buff)
        val message = BroadcastedMessage[Either[Block,Either[Transaction,Long]]](myId,Right(Left(TransactionCodec.decode(BitVector(buff)).require.value)),actualNumber.getAndIncrement())
        val encoded = SignedBroadcastMessageCodec.encode(message.sign(keyPair))
        sockets.foldRight(())((x,rec) =>{
          val output = x.getOutputStream
          output.write(implicitIntCodec.encode(encoded.require.toByteArray.length).require.toByteArray)
          output.write(encoded.require.toByteArray)
          rec
        })
      }
    }
  }

  class ServerSocketTransactionListen(s:ServerSocket) extends Runnable{
    override def run(): Unit = {
      while(true){
        val sock = s.accept()
        new Thread(new SocketTransactionListen(sock)).start()
      }
    }
  }

  class ListenSocket(s:Socket) extends Runnable{
    private def processMessage(value: Either[Block, Either[Transaction, Long]],encodedValue:Array[Byte]): Unit ={
      value match{
        case Left(b) => {
          node ! Utils.BlockReceived(b)
          sockets.foldRight(())((x,rec) =>{
            if(x!=s){
              x.getOutputStream.write(implicitIntCodec.encode(encodedValue.length).require.toByteArray)
              x.getOutputStream.write(encodedValue)
            }
            rec
          })
        }
        case Right(Left(t)) => {
          node ! Utils.TransactionReceived(t)
          sockets.foldRight(())((x,rec) =>{
            if(x!=s){
              x.getOutputStream.write(implicitIntCodec.encode(encodedValue.length).require.toByteArray)
              x.getOutputStream.write(encodedValue)
            }
            rec
          })
        }
        case Right(Right(i)) =>{
          val block = state.getBlock(i)
          if(block!=null){
            val encodedBlock = codec.encode(Left(block)).require.toByteArray
            s.getOutputStream.write(implicitIntCodec.encode(encodedBlock.length).require.toByteArray)
            s.getOutputStream.write(encodedBlock)
          }
        }
      }
    }
    override def run(): Unit = {
      val input = s.getInputStream
      val buffLength = new Array[Byte](4)
      while(true){
        input.read(buffLength)
        val messageLength = implicitIntCodec.decode(BitVector(buffLength)).require.value
        val buff = new Array[Byte](messageLength)
        input.read(buff)
        val signedBroadcast = SignedBroadcastMessageCodec.decode(BitVector(buff)).require.value
        if(signedBroadcast.verify()){
          val broadcasted = signedBroadcast.getMessage[Either[Block,Either[Transaction,Long]]](codec)
          val n = nextPerId.getOrElse(broadcasted.id,0:Long)
          if(n < broadcasted.number && broadcasted.number - n < 10){
            nextMessage.getOrElse(broadcasted.id,{val m = new mutable.HashMap[Long,Either[Block,Either[Transaction,Long]]]();nextMessage.put(broadcasted.id,m);m })
          }else if(n== broadcasted.number){
            processMessage(broadcasted.message,buff)
            var next = n + 1
            val dicc = nextMessage.getOrElse(broadcasted.id,{val m = new mutable.HashMap[Long,Either[Block,Either[Transaction,Long]]]();nextMessage.put(broadcasted.id,m);m })
            var elem = dicc.get(next)
            while(elem.isDefined){
              processMessage(elem.get,buff)
              next = next + 1
              elem = dicc.get(next)
            }
            nextPerId.put(broadcasted.id,next)
          }
        }
      }
    }
  }

  System.out.println("KNOW IPS")
  System.out.println(knowIps)

  val serverSocket = new ServerSocket()
  serverSocket.bind(myAddr)

  val onlyTransactionsSocket = new ServerSocket()
  onlyTransactionsSocket.bind(onlyTransactionAddr)

  new Thread(this).start()
  new Thread(new ServerSocketTransactionListen(onlyTransactionsSocket)).start()

  val actualNumber = new AtomicLong()

  override def receive: Receive = {
    case Utils.BlockReceived(n) =>{
      val brMessage = BroadcastedMessage(myId,Left(n):Either[Block,Either[Transaction,Long]],actualNumber.getAndIncrement()).sign(keyPair)(codec)
      for(s <- sockets){
        val encoded = SignedBroadcastMessageCodec.encode(brMessage).require.toByteArray
        s.getOutputStream.write(implicitIntCodec.encode(encoded.length).require.toByteArray)
        s.getOutputStream.write(encoded)
      }
    }

    case Utils.TransactionReceived(t) => {
      val brMessage = BroadcastedMessage(myId,Right(Left(t)):Either[Block,Either[Transaction,Long]],actualNumber.getAndIncrement()).sign(keyPair)(codec)
      for(s <- sockets){
        val encoded = SignedBroadcastMessageCodec.encode(brMessage).require.toByteArray
        s.getOutputStream.write(implicitIntCodec.encode(encoded.length).require.toByteArray)
        s.getOutputStream.write(encoded)
      }
    }
  }

  override def run(): Unit = {
    while(true){
      val s = serverSocket.accept()
      new Thread(new ListenSocket(s)).start()
      sockets = s :: sockets
    }
  }

  def obtainBlock(i:Long): Unit ={
    val s = sockets(random.nextInt() % sockets.size)
    val encoded = codec.encode(Right(Right(i))).require.toByteArray
    s.getOutputStream.write(implicitIntCodec.encode(encoded.length).require.toByteArray)
    s.getOutputStream.write(encoded)
  }
}