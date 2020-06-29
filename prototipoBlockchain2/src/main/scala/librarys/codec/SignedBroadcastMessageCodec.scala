package librarys.codec

import blockchain.BroadcastedMessageSigned
import librarys.crypto.ECDSASignature
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.codecs.implicits._

object SignedBroadcastMessageCodec extends Codec[BroadcastedMessageSigned]{
  override def decode(bits: BitVector): Attempt[DecodeResult[BroadcastedMessageSigned]] = {
    for{
      m <- implicitBitVectorCodec.decode(bits)
      r <- BigIntCodec.decode(m.remainder)
      s <- BigIntCodec.decode(r.remainder)
    }yield new DecodeResult[BroadcastedMessageSigned](BroadcastedMessageSigned(m.value,ECDSASignature(r.value,s.value)),s.remainder)
  }

  override def encode(value: BroadcastedMessageSigned): Attempt[BitVector] = {
    for{
      m <- implicitBitVectorCodec.encode(value.message)
      r <- BigIntCodec.encode(value.sign.r)
      s <- BigIntCodec.encode(value.sign.s)
    }yield m ++ r ++ s
  }

  override def sizeBound: SizeBound = SizeBound(implicitBitVectorCodec.sizeBound.lowerBound + BigIntCodec.sizeBound.lowerBound,None)
}
