import java.io.{File, RandomAccessFile}
import java.security.KeyPairGenerator

object GenerateGenesis extends App {
  val keyGen = KeyPairGenerator.getInstance("RSA")
  val pairKeys = keyGen.generateKeyPair
  val privateKey = pairKeys.getPrivate
  val publicKey = pairKeys.getPublic
  val zeroKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  val oneKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  val twoKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)

  val state = new State("genesis",false)
  val encodedPublicKey = if(publicKey.getEncoded.length<32) publicKey.getEncoded ++ Array.fill[Byte](32 - publicKey.getEncoded.length)(0) else publicKey.getEncoded

  val genesis = NodeBlockChain(zeroKey,List(SimpleTransaction(zeroKey,encodedPublicKey,0,0,10000,0,zeroKey)),RefundTransaction(zeroKey,0),Array(0,0,0,0,0,0,0,0),0)
  var actual = genesis
  state.nodes.put(ArrayDecorator(genesis.hashNode),genesis )
  state.sons.put(ArrayDecorator(genesis.hashNode),List())
  state.listHash.+=(ArrayDecorator(genesis.hashNode))
  state.founds.put(ArrayDecorator(encodedPublicKey),10000)
  state.close()

  val publicKeyFile = new RandomAccessFile(new File("genesisPublicKey"),"rw")
  val privateKeyFile = new RandomAccessFile(new File("genesisPrivateKey"),"rw")
  publicKeyFile.write(publicKey.getEncoded)
  privateKeyFile.write(privateKey.getEncoded)
  System.out.println(publicKey.getEncoded.size)
  publicKeyFile.close()
  privateKeyFile.close()
}
