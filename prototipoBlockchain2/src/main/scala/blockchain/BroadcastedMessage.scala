package blockchain

import librarys.crypto.ECDSASignature
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs.implicits.implicitLongCodec
import librarys.crypto
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

case class BroadcastedMessage[M](id:BitVector,message:M,number:Long){
  def sign(keyPair:AsymmetricCipherKeyPair)(implicit codec:Codec[M]): BroadcastedMessageSigned ={
    val encoded = id ++ codec.encode(message).require ++ implicitLongCodec.encode(number).require
    val s = crypto.ECDSASignature.sign(encoded.toByteArray,keyPair)
    BroadcastedMessageSigned(encoded,s)
  }
}

case class BroadcastedMessageSigned(message:BitVector,sign:ECDSASignature){
  def verify(): Boolean ={
    crypto.verify(message.toByteArray,sign,crypto.decodePublicKey(message.toByteArray.take(33)))
  }

  def getMessage[M](implicit codec:Codec[M]):BroadcastedMessage[M] = {
    val encoded = BitVector(message.toByteArray.drop(33))
    val m = codec.decode(encoded).require
    val number = implicitLongCodec.decode(m.remainder).require
    BroadcastedMessage[M](BitVector(message.toByteArray.take(33)),m.value,number.value)
  }
}
