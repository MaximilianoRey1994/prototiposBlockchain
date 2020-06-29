package blockchain

import java.math.BigInteger
import java.security.SecureRandom

import blockchain.StateTest.keyPairTr1A
import librarys.codec.{BlockCodec, ListCodec}
import librarys.crypto
import scodec.bits.BitVector
import librarys.codec.Codecs._
import scodec.codecs.implicits._

object StateTest extends App {
  val random = new SecureRandom()
  val keyPair = crypto.generateKeyPair(random)
  val minerUTXO = UTXO(BitVector(crypto.encodeKey(keyPair.getPublic)),100000)
  val genesis = Block(Array.fill[Byte](32)(1), List(),Array.fill[Byte](8)(0),0,minerUTXO)

  val state = new State(2,"./diskMap2",genesis)

  val keyPairTr1A = crypto.generateKeyPair(random)
  val keyPairTr1B = crypto.generateKeyPair(random)
  val keyPairTr1C = crypto.generateKeyPair(random)
  val utxo1A = UTXO(BitVector(crypto.encodeKey(keyPairTr1A.getPublic)),500)
  val utxo1B = UTXO(BitVector(crypto.encodeKey(keyPairTr1B.getPublic)),99500)
  val utxo1C = UTXO(BitVector(crypto.encodeKey(keyPairTr1C.getPublic)),5)
  val signedUTXO1 = minerUTXO.sign(List(utxo1A,utxo1B),keyPair)
  val transactions1 = List(Transaction(List(signedUTXO1),List(utxo1A,utxo1B)))

  val block = Block(genesis.hash,transactions1,Array.fill[Byte](8)(0),1,utxo1C)
  System.out.println(state.verifyAndAddBlockWithhoutPOF(block))

  val keyPairTr2A = crypto.generateKeyPair(random)
  val keyPairTr2B = crypto.generateKeyPair(random)

  val utxo2A = UTXO(BitVector(crypto.encodeKey(keyPairTr2A.getPublic)),99000)
  val utxo2B = UTXO(BitVector(crypto.encodeKey(keyPairTr2B.getPublic)),505)
  val utxo1BSigned = utxo1B.sign(List(utxo2A),keyPairTr1B)
  val transactions2 = List(Transaction(List(utxo1BSigned),List(utxo2A)))
  val block2 = Block(block.hash,transactions2,Array.fill[Byte](8)(0),2,utxo2B)

  System.out.println(state.verifyAndAddBlockWithhoutPOF(block2))

  val keyPairTr3A = crypto.generateKeyPair(random)
  val keyPairTr3B = crypto.generateKeyPair(random)
  val utxo3A = UTXO(BitVector(crypto.encodeKey(keyPairTr3A.getPublic)),1000)
  val utxo3B = UTXO(BitVector(crypto.encodeKey(keyPairTr3B.getPublic)),10)
  val signedUTXO2B = utxo2B.sign(List(utxo3A),keyPairTr2B)
  val signedUTXO1A = utxo1A.sign(List(utxo3A),keyPairTr1A)

  val block3 = Block(block2.hash,List(Transaction(List(signedUTXO1A,signedUTXO2B),List(utxo3A))),Array.fill[Byte](8)(3),3,utxo3B)

  val bi = BigInt(BigInteger.valueOf(6456457))
  if(!bi.equals(implicitBigInt.decode(implicitBigInt.encode(bi).require).require.value)) throw new RuntimeException("PROBLEM IN CODEC0")

  System.out.println(state.verifyAndAddBlockWithhoutPOF(block3))
  val decoded = BlockCodec.decode(BlockCodec.encode(block3).require).require.value
  if(!decoded.index.equals(block3.index)) throw new RuntimeException("PROBLEM IN CODEC1")
  if(!decoded.transaction.equals(block3.transaction)) throw new RuntimeException("PROBLEM IN CODEC2")
  if(!decoded.minerUTXO.equals(block3.minerUTXO)) throw new RuntimeException("PROBLEM IN CODEC3")

  System.out.println(state.verifyAndAddBlockWithhoutPOF(block3))
  System.out.println(state.verifyAndAddBlockWithhoutPOF(block))
  System.out.println(state.verifyAndAddBlockWithhoutPOF(block2))

  val keyPairTr4A = keyPairTr1A
  val keyPairTr4B = keyPairTr1B
  val utxo4A = UTXO(utxo1A.publicKey,8)
  val utxo4B = UTXO(utxo1B.publicKey,7)
  val signedUTXO3B = utxo3B.sign(List(utxo4A),keyPairTr3B)
  val block4 = Block(block3.hash,List(Transaction(List(signedUTXO3B),List(utxo4A))),Array.fill[Byte](8)(0),4,utxo4B)

  System.out.println(state.verifyAndAddBlockWithhoutPOF(block4))

  val keyPairTr5A = crypto.generateKeyPair(random)
  val keyPairTr5B = crypto.generateKeyPair(random)
  val keyPairTr5C = crypto.generateKeyPair(random)
  val utxo5A = UTXO(BitVector(crypto.encodeKey(keyPairTr5A.getPublic)),900)
  val utxo5B = UTXO(BitVector(crypto.encodeKey(keyPairTr5B.getPublic)),100)
  val utxo5C = UTXO(BitVector(crypto.encodeKey(keyPairTr5C.getPublic)),5)

  val signedUTXO3A = utxo3A.sign(List(utxo5A,utxo5B),keyPairTr3A)
  val block5 = Block(block4.hash,List(Transaction(List(signedUTXO3A),List(utxo5A,utxo5B))),Array.fill[Byte](8)(3),5,utxo5C)
  System.out.println(state.verifyAndAddBlockWithhoutPOF(block5))


  val utxoBadA = UTXO(utxo5C.publicKey,900)
  val utxoBadB = UTXO(BitVector(crypto.encodeKey(keyPairTr5B.getPublic)),5)
  val signedUtxo5A = utxo5A.sign(List(utxoBadA),keyPairTr5A)
  val badBlock = Block(block5.hash,List(Transaction(List(signedUtxo5A),List(utxoBadA))),Array.fill[Byte](8)(0),6,utxoBadB)
  System.out.println(state.verifyAndAddBlockWithhoutPOF(badBlock))
}
