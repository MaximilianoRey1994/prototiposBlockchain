package librarys.codec

import blockchain.{Block, Transaction}
import scodec.Attempt.Failure
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.implicits._
import librarys.codec.Codecs._


object BlockCodec extends Codec[Block]{
  val transactionListCodec = new ListCodec[Transaction]()( TransactionCodec )
  override def encode(value: Block): Attempt[BitVector] = {
    if(value.prevHash.length!=32) Failure(Err("hash with incorrect size"))
    else{
      for{
        transactions <- transactionListCodec.encode(value.transaction);
        nonce <- implicitBitVectorCodec.encode(BitVector(value.nonce))
        minerUTXO <- implicitUtxoCodec.encode(value.minerUTXO)
        index <- implicitLongCodec.encode(value.index)
      }yield BitVector(value.prevHash) ++ transactions ++ nonce ++ minerUTXO ++ index
    }
  }

  override def sizeBound: SizeBound = SizeBound(32+implicitUtxoCodec.sizeBound.lowerBound,None)

  override def decode(bits: BitVector): Attempt[DecodeResult[Block]] = {
    if(bits.toByteArray.length<32) Failure(Err("Hash have less of 32 bits"))
    else{
      val prevHash = bits.toByteArray.take(32)
      for{
        transactions <- transactionListCodec.decode(BitVector(bits.toByteArray.drop(32)))
        nonce <- implicitBitVectorCodec.decode(transactions.remainder)
        minerUTXO <- implicitUtxoCodec.decode(nonce.remainder)
        index <- implicitLongCodec.decode(minerUTXO.remainder)
      }yield new DecodeResult[Block](Block(prevHash,transactions.value,nonce.value.toByteArray,index.value,minerUTXO.value),index.remainder)
    }
  }
}
