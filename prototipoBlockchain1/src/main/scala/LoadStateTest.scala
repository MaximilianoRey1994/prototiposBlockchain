import java.security.KeyPairGenerator

object LoadStateTest extends App{
  val keyGen = KeyPairGenerator.getInstance("RSA")
  val pairKeys = keyGen.generateKeyPair
  val privateKey = pairKeys.getPrivate
  val publicKey = pairKeys.getPublic
  val zeroKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  val oneKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  val twoKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)

  val state = new State("prueba",true)
  System.out.println("TAMAÑO: " + state.nodes.size)
  System.out.println(state.foundOf(zeroKey))
}
