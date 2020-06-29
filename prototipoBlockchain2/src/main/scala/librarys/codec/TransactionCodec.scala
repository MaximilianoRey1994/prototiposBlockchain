package librarys.codec

import blockchain.{SignedUTXO, Transaction, UTXO}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import librarys.codec.Codecs._

object TransactionCodec extends Codec[Transaction]{
  val listCodecSignedUTXOs = new ListCodec[SignedUTXO]()(implicitSignedUtxoCodec)
  val listCodecUTXOs = new ListCodec[UTXO]()(implicitUtxoCodec)
  override def encode(value: Transaction): Attempt[BitVector] = {
    for{
      inputs <- listCodecSignedUTXOs.encode(value.inputs)
      outputs <- listCodecUTXOs.encode(value.outputs)
    }yield inputs ++ outputs
  }

  override def sizeBound: SizeBound = SizeBound(listCodecSignedUTXOs.sizeBound.lowerBound + listCodecUTXOs.sizeBound.lowerBound,None)

  override def decode(bits: BitVector): Attempt[DecodeResult[Transaction]] = {
    for{
      inputs <- listCodecSignedUTXOs.decode(bits)
      outputs <- listCodecUTXOs.decode(inputs.remainder)
    }yield new DecodeResult[Transaction](Transaction(inputs.value,outputs.value),outputs.remainder)
  }
}
