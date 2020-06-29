package librarys.codec

import java.math.BigInteger

import blockchain.{SignedUTXO, UTXO}
import librarys.crypto.ECDSASignature
import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.implicits._

object Codecs {
  implicit val implicitUtxoCodec = new Codec[UTXO] {
    override def encode(value: UTXO): Attempt[BitVector] = {
      if(value.publicKey.toByteArray.length!=33) Failure(Err("MALFORMED UTXO: PUBLIC KEY"))
      for{
        v <- implicitLongCodec.encode( value.value)
      }yield value.publicKey ++ v
    }

    override def sizeBound: SizeBound = SizeBound(33+8+8,Some(33+8+8))

    override def decode(bits: BitVector): Attempt[DecodeResult[UTXO]] = {
      if(bits.toByteArray.length < 33) return Failure(Err("Wrong length of public key"))
      val publicKey = bits.take(33*8)
      val remaind = bits.drop(33*8)
      for{
        v <- implicitLongCodec.decode(remaind)
      }yield new DecodeResult[UTXO](UTXO(publicKey,v.value),v.remainder)
    }
  }

  implicit val implicitBigInt = new Codec[BigInt] {
    override def encode(value: BigInt): Attempt[BitVector] = implicitBitVectorCodec.encode(BitVector(value.toByteArray))

    override def sizeBound: SizeBound = SizeBound(implicitBitVectorCodec.sizeBound.lowerBound,None)

    override def decode(bits: BitVector): Attempt[DecodeResult[BigInt]] = {
      for{
        r <- implicitBitVectorCodec.decode(bits)
      }yield new DecodeResult[BigInt](new BigInt(new BigInteger(r.value.toByteArray)),r.remainder)
    }
  }
  implicit val implicitECDSASignatureCodec = new Codec[ECDSASignature] {
    override def encode(value: ECDSASignature): Attempt[BitVector] = {
      for{
        r <- implicitBigInt.encode(value.r)
        s <- implicitBigInt.encode(value.s)
      }yield r ++ s

    }

    override def sizeBound: SizeBound = SizeBound(16,Some(16))

    override def decode(bits: BitVector): Attempt[DecodeResult[ECDSASignature]] = {
      for{
        r <- implicitBigInt.decode(bits)
        s <- implicitBigInt.decode(r.remainder)
      }yield new DecodeResult[ECDSASignature](ECDSASignature(r.value,s.value),s.remainder)
    }
  }
  implicit val implicitSignedUtxoCodec = new Codec[SignedUTXO] {
    override def encode(value: SignedUTXO): Attempt[BitVector] = {
      for{
        utxo <- implicitUtxoCodec.encode(value.utxo)
        sign <- implicitECDSASignatureCodec.encode(value.sign)
      }yield utxo++sign
    }

    override def sizeBound: SizeBound = SizeBound(33+8+8+16,Some(33+8+8+16))

    override def decode(bits: BitVector): Attempt[DecodeResult[SignedUTXO]] = {
      for{
        utxo <- implicitUtxoCodec.decode(bits)
        sign <- implicitECDSASignatureCodec.decode(utxo.remainder)
      }yield new DecodeResult[SignedUTXO](SignedUTXO(utxo.value,sign.value),sign.remainder)
    }
  }
}
