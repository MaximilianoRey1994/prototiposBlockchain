package librarys.codec

import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.implicits.implicitIntCodec

import scala.annotation.tailrec

class ListCodec[A](implicit codec:Codec[A]) extends Codec[List[A]]{
  override def encode(value: List[A]): Attempt[BitVector] = {
    implicitIntCodec.encode(value.size) match {
      case Failure(err) => Failure(err)
      case Successful(length) => {
        @tailrec
        def aux(l:List[A],res:BitVector): Attempt[BitVector] = l match{
          case Nil => Successful(length ++ res)
          case el::rec => codec.encode(el) match{
            case Failure(err) => Failure(err)
            case Successful(encodedEl) => aux(rec,encodedEl ++ res)
          }
        }
        aux(value,BitVector(new Array[Byte](0)))
      }
    }
  }

  override def sizeBound: SizeBound = SizeBound(4,None)

  override def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] = {
    implicitIntCodec.decode(bits) match {
      case Failure(err) => Failure(err)
      case Successful(length) => {
        @tailrec
        def aux(elems:Int,res:List[A],remainder:BitVector): Attempt[DecodeResult[List[A]]] = {
          if(elems.equals(length.value)) Successful(new DecodeResult[List[A]](res,remainder))
          else{
            codec.decode(remainder) match{
              case Failure(err) => Failure(err)
              case Successful(v) => aux(elems+1,v.value :: res,v.remainder)
            }
          }
        }
        aux(0,Nil,length.remainder)
      }
    }
  }
}
