import java.nio.ByteBuffer

import io.iohk.decco.{BufferInstantiator, CodecContract}
import io.iohk.decco.Codec.{DecodeResult, Failure}

import scala.annotation.tailrec

object ByteArrayCodecContract extends CodecContract[Array[Byte]] {
  override def size(t: Array[Byte]): Int = t.length + 4

  override def encodeImpl(t: Array[Byte], start: Int, destination: ByteBuffer): Unit = {
    IntegerCodecContract.encodeImpl(t.length,start,destination)
    t.foldLeft(start+4)((pos,elem) => {destination.put(pos,elem);pos+1} )
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Array[Byte]]] = {
    IntegerCodecContract.decodeImpl(start,source) match{
      case Left(f) => Left(f)
      case Right(size) => {
        val res = new Array[Byte](size.decoded)
        if(source.capacity() < size.nextIndex+size.decoded) {return Left(Failure)}
        for(i <- 0 until size.decoded){
          res(i) = source.get(i+size.nextIndex)
        }
        Right(new DecodeResult[Array[Byte]](res,size.decoded+size.nextIndex))
      }
    }
  }
}


object IntegerCodecContract extends CodecContract[Int]{
  def size(t: Int): Int = 4

  def encodeImpl(t: Int, start: Int, destination: ByteBuffer): Unit = {
    destination.putInt(start,t)
  }

  def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Int]] = {
    if(start+4>source.capacity()) Left(Failure)
    else Right(new DecodeResult[Int](source.getInt(start),start+4))
  }
}

object LongerStreamObject extends CodecContract[Long]{
  override def size(t: Long): Int = 8

  override def encodeImpl(t: Long, start: Int, destination: ByteBuffer): Unit = {
    destination.putLong(start,t)
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Long]] = {
    if(start+8>source.capacity()) Left(Failure)
    else Right(new DecodeResult[Long](source.getLong(start),start+8))
  }
}

class EitherCodecContract[A,B](a:CodecContract[A],b:CodecContract[B]) extends CodecContract[Either[A,B]]{

  override def size(t: Either[A, B]): Int = t match{
    case Left(elemA) => a.size(elemA) + 1
    case Right(elemB) => b.size(elemB) + 1
  }

  override def encodeImpl(t: Either[A, B], start: Int, destination: ByteBuffer): Unit = t match{
    case Left(elemA) =>{
      destination.put(start,0)
      a.encodeImpl(elemA,start+1,destination)
    }
    case Right(elemB) => {
      destination.put(start,1)
      b.encodeImpl(elemB,start+1,destination)
    }
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Either[A, B]]] = {
    if(source.get(start)==0){
      val rec = a.decodeImpl(start+1,source)
      rec match {
        case Left(f) => Left(f)
        case Right(dec) => Right( new DecodeResult[Either[A, B]](Left(dec.decoded),dec.nextIndex))
      }
    }else if(source.get(start)==1){
      val rec = b.decodeImpl(start+1,source)
      rec match {
        case Left(f) => Left(f)
        case Right(dec) => Right( new DecodeResult[Either[A, B]](Right(dec.decoded),dec.nextIndex))
      }
    }else{
      Left(Failure)
    }
  }
}

object CharacterCodecContract extends CodecContract[Char]{

  override def size(t: Char): Int = 1

  override def encodeImpl(t: Char, start: Int, destination: ByteBuffer): Unit = destination.putChar(start,t)

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Char]] = {
    if(start>source.capacity()) Left(Failure)
    else Right(new DecodeResult[Char](source.getChar(start),start+1))

  }
}

class ListCodecContract[A](CodecContract: CodecContract[A]) extends CodecContract[List[A]]{

  override def size(t: List[A]): Int = t.foldLeft(4)((rec,x) => CodecContract.size(x) + rec)

  override def encodeImpl(t: List[A], start: Int, destination: ByteBuffer): Unit = {
   // Extras.copyIntToArrayBuffer(destination,t.size,start)
    IntegerCodecContract.encodeImpl(t.size,start,destination)
    //System.out.println("PRIMER SEGMENTO: " + destination.get(start) + " REAL SIZE: " + t.size + " SIZE: " + Extras.copyArrayBufferToInt(destination,start))
    var ind = start + 4
    for(el <- t ){
      CodecContract.encodeImpl(el,ind,destination)
      ind = ind + CodecContract.size(el)
    }
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[List[A]]] = {
    IntegerCodecContract.decodeImpl(start,source) match {
      case Left(f) => Left(f)
      case Right(_dec) => {
        @tailrec
        def aux(pos:Int,number:Int,acum:List[A]): Either[Failure, DecodeResult[List[A]]] = {
          if(number>=_dec.decoded) Right(new DecodeResult[List[A]](acum.reverse,pos))
          else{
            val dec = CodecContract.decodeImpl(pos,source)
            dec match {
              case Left(f) => Left(f)
              case Right(v) => aux(v.nextIndex,number+1,v.decoded :: acum)
            }
          }
        }
        aux(_dec.nextIndex,0,Nil)
      }
    }
  }
}

object SimpleTransactionCodecContract extends CodecContract[SimpleTransaction]{
  override def size(t: SimpleTransaction): Int = 8*4 + ByteArrayCodecContract.size(t.srcValidation) + ByteArrayCodecContract.size(t.src) + ByteArrayCodecContract.size(t.dst)

  override def encodeImpl(t: SimpleTransaction, start: Int, destination: ByteBuffer): Unit = {
    LongerStreamObject.encodeImpl(t.mountToMiner,start,destination)
    LongerStreamObject.encodeImpl(t.srcRest,start+8,destination)
    LongerStreamObject.encodeImpl(t.dstRest,start+8+8,destination)
    LongerStreamObject.encodeImpl(t.index,start+8+8+8,destination)
    ByteArrayCodecContract.encodeImpl(t.srcValidation,start+8+8+8+8,destination)
    ByteArrayCodecContract.encodeImpl(t.src,start+8+8+8+8+ByteArrayCodecContract.size(t.srcValidation),destination)
    ByteArrayCodecContract.encodeImpl(t.dst,start+8+8+8+8+ByteArrayCodecContract.size(t.srcValidation)+ByteArrayCodecContract.size(t.src),destination)
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[SimpleTransaction]] = {
    LongerStreamObject.decodeImpl(start,source) match{
      case Left(f) => Left(f)
      case Right(mountToMiner) => LongerStreamObject.decodeImpl(mountToMiner.nextIndex,source) match{
        case Left(f) => Left(f)
        case Right(srcRest) => LongerStreamObject.decodeImpl(srcRest.nextIndex,source) match {
          case Left(f) => Left(f)
          case Right(dstRes) => LongerStreamObject.decodeImpl(dstRes.nextIndex,source) match{
            case Left(f) => Left(f)
            case Right(index) => ByteArrayCodecContract.decodeImpl(index.nextIndex,source) match{
              case Left(f) => Left(f)
              case Right(srcValidation) => ByteArrayCodecContract.decodeImpl(srcValidation.nextIndex,source) match{
                case Left(f) => Left(f)
                case Right(src) => ByteArrayCodecContract.decodeImpl(src.nextIndex,source) match{
                  case Left(f) => Left(f)
                  case Right(dst) => Right(new DecodeResult[SimpleTransaction](SimpleTransaction(src.decoded,dst.decoded,mountToMiner.decoded,srcRest.decoded,dstRes.decoded,index.decoded,srcValidation.decoded),dst.nextIndex))
                }
              }
            }
          }
        }
      }
    }
  }
}

object RefundTransactionCodecContract extends CodecContract[RefundTransaction]{

  override def size(t: RefundTransaction): Int = 8 + ByteArrayCodecContract.size(t.dst)

  override def encodeImpl(t: RefundTransaction, start: Int, destination: ByteBuffer): Unit = {
    LongerStreamObject.encodeImpl(t.dstRest,start,destination)
    ByteArrayCodecContract.encodeImpl(t.dst,start+8,destination)
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[RefundTransaction]] = {
    LongerStreamObject.decodeImpl(start,source) match{
      case Left(f) => Left(f)
      case Right(dstRes) => ByteArrayCodecContract.decodeImpl(dstRes.nextIndex,source) match {
        case Left(f) => Left(f)
        case Right(dst) => Right(new DecodeResult[RefundTransaction](RefundTransaction(dst.decoded,dstRes.decoded),dst.nextIndex))
      }
    }
  }
}

object QuestStream extends CodecContract[Quest]{

  override def size(t: Quest): Int = 4

  override def encodeImpl(t: Quest, start: Int, destination: ByteBuffer): Unit = IntegerCodecContract.encodeImpl(t.numberNode,start,destination)

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Quest]] = {
    if(start+4 >= source.capacity()) Left(Failure)
    else Right(new DecodeResult[Quest](Quest(IntegerCodecContract.decodeImpl(start,source).right.get.decoded),start+4))
  }
}

object NodeCodecContract extends CodecContract[NodeBlockChain]{
  val streamList = new ListCodecContract[SimpleTransaction](SimpleTransactionCodecContract)

  override def size(t: NodeBlockChain): Int = 8+ByteArrayCodecContract.size(t.prev_hash)+RefundTransactionCodecContract.size(t.refundTransaction)+ByteArrayCodecContract.size(t.nonce) + streamList.size(t.transactions)

  override def encodeImpl(t: NodeBlockChain, start: Int, destination: ByteBuffer): Unit = {
    LongerStreamObject.encodeImpl(t.index,start,destination)
    ByteArrayCodecContract.encodeImpl(t.prev_hash,start+8,destination)
    RefundTransactionCodecContract.encodeImpl(t.refundTransaction,start+ 8+ ByteArrayCodecContract.size(t.prev_hash),destination)
    ByteArrayCodecContract.encodeImpl(t.nonce,start+8+ByteArrayCodecContract.size(t.prev_hash)+RefundTransactionCodecContract.size(t.refundTransaction),destination)
    streamList.encodeImpl(t.transactions,start+8+ByteArrayCodecContract.size(t.prev_hash)+RefundTransactionCodecContract.size(t.refundTransaction)+ByteArrayCodecContract.size(t.nonce),destination)
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[NodeBlockChain]] = {
    LongerStreamObject.decodeImpl(start,source) match{
      case Left(f) => Left(f)
      case Right(index) => ByteArrayCodecContract.decodeImpl(index.nextIndex,source) match{
        case Left(f) => Left(f)
        case Right(prev_hash) => RefundTransactionCodecContract.decodeImpl(prev_hash.nextIndex,source) match{
          case Left(f) => Left(f)
          case Right(refundTransaction) => ByteArrayCodecContract.decodeImpl(refundTransaction.nextIndex,source) match{
            case Left(f) => Left(f)
            case Right(nonce) => streamList.decodeImpl(nonce.nextIndex,source) match{
              case Left(f) => Left(f)
              case Right(transactions) => Right(new DecodeResult[NodeBlockChain](NodeBlockChain(prev_hash.decoded,transactions.decoded,refundTransaction.decoded,nonce.decoded,index.decoded),transactions.nextIndex))
            }
          }
        }
      }
    }
  }
}

object StringCodecContract extends CodecContract[String]{

  override def size(t: String): Int = t.size + 1

  override def encodeImpl(t: String, start: Int, destination: ByteBuffer): Unit = {
    for(i <- 0 to (t.size - 1)){
      destination.put(i,t(i).toByte)
    }
    destination.put(t.size,0)
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[String]] = {
    val sb = new StringBuilder
    @tailrec
    def aux(i:Int):Either[Failure, DecodeResult[String]] = {
      if(i>source.capacity()) Left(Failure)
      else if(source.get(i)!=0) {
        sb.append(source.get(i).toChar)
        aux(i+1)
      }else{
        val res = sb.toString()
        Right(new DecodeResult[String](res,start+res.size + 1))
      }
    }
    aux(start)
  }
}

object Byte32CodecContract extends CodecContract[Array[Byte]]{

  override def size(t: Array[Byte]): Int = 32

  override def encodeImpl(t: Array[Byte], start: Int, destination: ByteBuffer): Unit = {
    for(i <- 0 to 31){
      destination.put(start+i,t(i))
    }
  }

  override def decodeImpl(start: Int, source: ByteBuffer): Either[Failure, DecodeResult[Array[Byte]]] = {
    if(start+32>source.capacity()) Left(Failure)
    else{
      val res = new Array[Byte](32)
      for(i <- 0 to 31){
        res(i) = source.get(start+i)
      }
      Right(new DecodeResult[Array[Byte]](res,start+32))
    }
  }
}