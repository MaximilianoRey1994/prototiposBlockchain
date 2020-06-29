package librarys.codec

import java.math.BigInteger

import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.codecs.implicits._

object BigIntCodec extends Codec[BigInt]{
  override def decode(bits: BitVector): Attempt[DecodeResult[BigInt]] = implicitBitVectorCodec.decode(bits) match{
    case Failure(err) => Failure(err)
    case Successful(v) => Successful(DecodeResult[BigInt](new BigInt(new BigInteger(v.value.toByteArray)),v.remainder))
  }

  override def encode(value: BigInt): Attempt[BitVector] = implicitBitVectorCodec.encode(BitVector(value.toByteArray))

  override def sizeBound: SizeBound = SizeBound(implicitBitVectorCodec.sizeBound.lowerBound,None)
}
