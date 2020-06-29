package wallet

import java.net.{InetSocketAddress, ServerSocket, Socket}

import blockchain.{Transaction, UTXO}
import librarys.codec.TransactionCodec
import scodec.bits.BitVector
import scodec.codecs.implicits._
import  librarys.codec.Codecs._

class WalletProxy(nodeAddr:InetSocketAddress,walletAddr:InetSocketAddress,wallet:Wallet) extends Runnable{
  val socket = new Socket(nodeAddr.getAddress,nodeAddr.getPort)
  val socketOutput = socket.getOutputStream
  val serverSocket = new ServerSocket()
  serverSocket.bind(walletAddr)
  def sendTransaction(t:Transaction):Unit = {
    val encoded = TransactionCodec.encode(t).require.toByteArray
    //System.out.println("WRITE8: " + encoded.length)
    socketOutput.write(implicitIntCodec.encode(encoded.length).require.toByteArray)
    socketOutput.write(encoded)
  }

  override def run(): Unit = {
    while(true){
      val s = serverSocket.accept()
      val inputS = s.getInputStream
      val outputS = s.getOutputStream
      val mountEncoded = new Array[Byte](8)
      inputS.read(mountEncoded)
      val mount = implicitLongCodec.decode(BitVector(mountEncoded)).require.value
      val n = wallet.newUTXO(mount)
      val encodedUTXO = implicitUtxoCodec.encode(n._1).require
      outputS.write(encodedUTXO.toByteArray)
    }
  }
  new Thread(this).start()

  def sendMoney(i:Long,ip:InetSocketAddress):UTXO = {
    val s = new Socket(ip.getAddress,ip.getPort)
    val outputS = s.getOutputStream
    val inputS = s.getInputStream
    outputS.write(implicitLongCodec.encode(i).require.toByteArray)
    val response = new Array[Byte](implicitUtxoCodec.sizeBound.lowerBound.toInt)
    inputS.read(response)
    implicitUtxoCodec.decode(BitVector(response)).require.value
  }
}
