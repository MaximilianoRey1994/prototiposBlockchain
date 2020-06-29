import java.io.File
import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.security.{KeyFactory, MessageDigest, PrivateKey, PublicKey}
import java.security.spec.PKCS8EncodedKeySpec
import java.nio.file.Files
import java.security.spec.X509EncodedKeySpec
import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import javax.crypto.Cipher

import scala.collection.parallel.ForkJoinTasks
import org.scalameter._

import scala.util.DynamicVariable

abstract class Transaction{
  def length:Long
  val mountToMiner:Long
  val getBytes:Array[Byte]
  def newStates:List[(Array[Byte],Long)]
}


object Extras {
  def hashingNode(n:NodeBlockChain): Array[Byte] ={
    val arr = n.prev_hash ++ n.transactions.foldRight(Array():Array[Byte])((x, rec) => x.getBytes ++ rec) ++ n.refundTransaction.getBytes ++ n.nonce ++ Extras.longToByte(n.index)
    val d = MessageDigest.getInstance("SHA-256")
    d.digest(arr)
  }
  def arrayToString(arr: Array[Byte]): String = {
    "[" ++ arr.foldRight("]")((x,rec) => "0x" ++ x.toHexString ++ "," ++rec)
    //"[" ++ arr.mkString("+") + "]"
  }

  val simpleTransactionLength = 294 + 294 + 8 + 8 + 8 + 32
  val refundTransactionLength = 8 + 294
  val maxTransactionInNode = 10
  val maxNodeLength = simpleTransactionLength * maxTransactionInNode + refundTransactionLength + 32 + 8 + 8

  def longToByte(l: Long): Array[Byte] = {
    val res = ByteBuffer.allocate(8)
    LongerStreamObject.encodeImpl(l,0,res)
    res.array()
  }

  def bytesToLong(arr: Array[Byte]): Long = {
    val res = ByteBuffer.wrap(arr)
    LongerStreamObject.decodeImpl(0,res).right.get.decoded
  }

  def intToByte(l: Int): Array[Byte] = {
    val res = new Array[Byte](4)

    @tailrec
    def intToByteAux(i: Int): Unit = {
      if (i < 4) {
        res(i) = ((l >> i) % 256).toByte
        intToByteAux(i + 1)
      }
    }

    intToByteAux(0)
    res
  }

  def bytesToInt(arr: Array[Byte]): Int = {
    var res: Int = 0

    @tailrec
    def bytesToIntAux(i: Int): Unit = {
      if (i < 4) {
        val r: Int = arr(i)
        res = res + (r << i)
        bytesToIntAux(i + 1)
      }
    }

    bytesToIntAux(0)
    res
  }

  @tailrec
  def copyArray(dst: Array[Byte], src: Array[Byte], dst_i: Int, src_i: Int, length: Int): Unit = {
    if (length > 0) {
      dst(dst_i + length - 1) = src(src_i + length - 1)
      copyArray(dst, src, dst_i, src_i, length - 1)
    }
  }

  def hashLength = 32

  def messageLengthStandar = 32

  def nonceLength = 8


  def equalsZip(arr: Array[(Byte, Byte)]): Boolean = {
    arr.foldRight(true)((x, rec) => if (x._1 != x._2) false else rec)
  }

  object CorruptedMessageError extends RuntimeException;

  def decrypt(key: PublicKey, data: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE, key)
    cipher.doFinal(data)
  }

  def encrypt(key: PrivateKey, data: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    cipher.doFinal(data)
  }

  //------------------------------COPIADO DE CURSOS---------------------------------------
  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler = new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)
  val forkJoinPool = new ForkJoinPool

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }

  //-------------------------FIN COPIADO DE CURSOS---------------------------------------


  @throws[Exception]
  def getPrivateKey(filename: String): PrivateKey = {
    val keyBytes = Files.readAllBytes(new File(filename).toPath)
    val spec = new PKCS8EncodedKeySpec(keyBytes)
    val kf = KeyFactory.getInstance("RSA")
    kf.generatePrivate(spec)
  }


  @throws[Exception]
  def getPublicKey(filename: String): PublicKey = {
    val keyBytes = Files.readAllBytes(new File(filename).toPath)
    val spec = new X509EncodedKeySpec(keyBytes)
    val kf = KeyFactory.getInstance("RSA")
    kf.generatePublic(spec)
  }

  def generateTransaction(src:Array[Byte],dst:Array[Byte],mountToMiner:Long,srcRest:Long,dstRest:Long,index:Long,privateKey: PrivateKey) ={
    val simpleTransaction:SimpleTransaction = SimpleTransaction(src,dst,mountToMiner,srcRest,dstRest,index,Array())
    SimpleTransaction(src,dst,mountToMiner,srcRest,dstRest,index,encrypt(privateKey,simpleTransaction.hashWithoutValidation))
  }

  def copyArrayToByteBuffer(dst: ByteBuffer, arr: Array[Byte], j: Int, size: Int): Unit = {
    @tailrec
    def aux(i: Int, j: Int, act: Int): Unit = {
      if (act < size) {
        dst.put(j, arr(i))
        aux(i + 1, j + 1, act + 1)
      }
    }

    aux(0, j, 0)
  }

  def copyBufferToArray(src: ByteBuffer, j: Int, size: Int): Array[Byte] = {
    val dst: Array[Byte] = new Array[Byte](size)

    @tailrec
    def aux(i: Int, j: Int, act: Int): Unit = {
      if (act < size) {
        dst(i) = src.get(j)
        aux(i + 1, j + 1, act + 1)
      }
    }

    aux(0, j, 0)
    dst
  }
}
case class NewTailNode(n:NodeBlockChain)
case class NewTransaction(t:SimpleTransaction)


case class Quest(numberNode:Int)