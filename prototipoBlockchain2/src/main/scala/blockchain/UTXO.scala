package blockchain

import librarys.crypto
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import scodec.bits.BitVector
import librarys.codec.Codecs.implicitUtxoCodec
import scodec.codecs.implicits._

case class UTXO(publicKey:BitVector,value:Long){
  def sign(outs:List[UTXO],keyPair:AsymmetricCipherKeyPair): SignedUTXO ={
    if(!publicKey.equals(BitVector(crypto.encodeKey(keyPair.getPublic)))) throw new RuntimeException("KEY PAIR INCORRECT")
    val sign = crypto.ECDSASignature.sign((implicitUtxoCodec.encode(this).require ++ outs.foldRight(BitVector(new Array[Byte](0)))((x,rec) => x.publicKey ++ implicitLongCodec.encode(x.value).require ++ rec )).toByteArray,keyPair)
    SignedUTXO(this,sign)
  }

  override def equals(obj: Any): Boolean = {
    if(!this.getClass.equals(obj.getClass)) false
    else {
      val cast_obj = obj.asInstanceOf[UTXO]
      this.publicKey.equals(cast_obj.publicKey) && this.value.equals(cast_obj.value)
    }
  }

  override def hashCode(): Int = this.publicKey.hashCode
}
