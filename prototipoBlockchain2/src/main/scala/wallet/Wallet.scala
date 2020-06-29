package wallet

import java.net.InetSocketAddress
import java.security.SecureRandom

import blockchain.{SignedUTXO, Transaction, UTXO}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import librarys.crypto
import scodec.bits.BitVector

import scala.annotation.tailrec

class Wallet(nodeIp:InetSocketAddress,walletAddr:InetSocketAddress,knowUTXOs:List[(UTXO,AsymmetricCipherKeyPair)]) {
  private var utxos:List[(UTXO,AsymmetricCipherKeyPair)] = knowUTXOs
  private val random = new SecureRandom()
  private val walletProxy = new WalletProxy(nodeIp,walletAddr,this)


  def newUTXO(v:Long):(UTXO,AsymmetricCipherKeyPair) = synchronized({
    val key = crypto.generateKeyPair(random)
    val res = (UTXO(BitVector(crypto.encodeKey(key.getPublic)),v),key)
    utxos = res :: utxos
    res
  })

  def inputFor(outputs:List[UTXO],feed:Long): (List[SignedUTXO],List[UTXO]) = synchronized({
    var actualInput:Long = 0
    val value = outputs.foldRight(feed)((x,rec) => x.value + rec)
    @tailrec
    def aux(listInputs:List[(UTXO,AsymmetricCipherKeyPair)],res:Int):Int ={
      if(actualInput>=value) res
      else{
        listInputs match{
          case Nil => throw new RuntimeException("NOT FOUNDS ENOUGHT")
          case u::ls => {
            actualInput += u._1.value
            aux(ls,res+1)
          }
        }
      }
    }
    val index = aux(utxos,0)
    val inp = utxos.take(index)
    utxos = utxos.drop(index)
    val valueInp = inp.foldRight(0:Long)((x,rec) => x._1.value + rec)
    var out = outputs
    if(valueInp != value){
      val nUtxo = newUTXO(valueInp - value)
      out = nUtxo._1 :: out
    }

    (inp.map(x => x._1.sign(out,x._2)),out)
  })

  def sendTo(output:List[UTXO],feed:Long): Unit ={
    val inp = inputFor(output,feed)
    val tr = Transaction(inp._1,inp._2)
    walletProxy.sendTransaction(tr)
  }

  def sendToWallet(ip:InetSocketAddress,value:Long,feed:Long):Unit = {
    val utxo = walletProxy.sendMoney(value,ip)
    sendTo(List(utxo),feed)
  }
}
