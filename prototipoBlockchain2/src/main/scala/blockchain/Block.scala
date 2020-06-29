package blockchain

import java.security.MessageDigest

import librarys.codec.BlockCodec

case class Block(prevHash:Array[Byte], transaction:List[Transaction],nonce:Array[Byte],index:Long,minerUTXO:UTXO) {
  lazy val hash = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(BlockCodec.encode(this).require.toByteArray)
    digest.digest()
  }
}
