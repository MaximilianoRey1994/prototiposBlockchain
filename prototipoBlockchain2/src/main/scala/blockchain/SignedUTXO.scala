package blockchain

import librarys.crypto
import librarys.crypto.ECDSASignature
import scodec.codecs.implicits.implicitLongCodec
import librarys.codec.Codecs.implicitUtxoCodec
import scodec.bits.BitVector

case class SignedUTXO(utxo: UTXO,sign:ECDSASignature) {
  def verify(outs:List[UTXO]): Boolean ={
    crypto.verify((implicitUtxoCodec.encode(utxo).require ++ outs.foldRight(BitVector(new Array[Byte](0)))((x,rec) => x.publicKey ++ implicitLongCodec.encode(x.value).require ++ rec )).toByteArray,sign,crypto.decodePublicKey(utxo.publicKey.toByteArray))
  }
}
