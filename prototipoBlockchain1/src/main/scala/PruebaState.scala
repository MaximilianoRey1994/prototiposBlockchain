
object PruebaState extends App {

  import java.security.KeyPair
  import java.security.PrivateKey
  import java.security.PublicKey
  import java.security.KeyPairGenerator

  val keyGen = KeyPairGenerator.getInstance("RSA")
  val pairKeys = keyGen.generateKeyPair
  val privateKey = pairKeys.getPrivate
  val publicKey = pairKeys.getPublic
  val zeroKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  val oneKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  val twoKey = Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)

  val state = new State("prueba",false)
  val encodedPublicKey = if(publicKey.getEncoded.length<32) publicKey.getEncoded ++ Array.fill[Byte](32 - publicKey.getEncoded.length)(0) else publicKey.getEncoded

  val genesis = NodeBlockChain(zeroKey,List(SimpleTransaction(zeroKey,encodedPublicKey,0,0,10000,0,zeroKey)),RefundTransaction(zeroKey,0),Array(0,0,0,0,0,0,0,0),0)
  var actual = genesis
  state.nodes.put(ArrayDecorator(genesis.hashNode),genesis )
  state.sons.put(ArrayDecorator(genesis.hashNode),List())
  state.listHash.+=(ArrayDecorator(genesis.hashNode))
  state.founds.put(ArrayDecorator(encodedPublicKey),10000)
  actual = NodeBlockChain(actual.hashNode,List(Extras.generateTransaction(encodedPublicKey,oneKey,20,8980,1000,1,privateKey)),RefundTransaction(zeroKey,25),Array(0,0,0,0,0,0,0,0),1)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println(state.foundOf(encodedPublicKey))
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))
  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,30),Array(0,0,0,0,0,0,0,0),2)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,35),Array(0,0,0,0,0,0,0,0),3)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,40),Array(0,0,0,0,0,0,0,0),4)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,45),Array(0,0,0,0,0,0,0,0),5)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,50),Array(0,0,0,0,0,0,0,0),6)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,55),Array(0,0,0,0,0,0,0,0),7)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,60),Array(0,0,0,0,0,0,0,0),8)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,65),Array(0,0,0,0,0,0,0,0),9)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,70),Array(0,0,0,0,0,0,0,0),10)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,75),Array(0,0,0,0,0,0,0,0),11)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,80),Array(0,0,0,0,0,0,0,0),12)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,85),Array(0,0,0,0,0,0,0,0),13)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,90),Array(0,0,0,0,0,0,0,0),14)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,95),Array(0,0,0,0,0,0,0,0),15)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

    var ant = actual
  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,100),Array(0,0,0,0,0,0,0,0),16)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,105),Array(0,0,0,0,0,0,0,0),17)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  ant = NodeBlockChain(ant.hashNode,List(),RefundTransaction(twoKey,5),Array(0,0,0,0,0,0,0,0),16)
  if(state.receiveNode(ant)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))

  actual = NodeBlockChain(actual.hashNode,List(),RefundTransaction(zeroKey,110),Array(0,0,0,0,0,0,0,0),18)
  if(state.receiveNode(actual)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Founds of zero: " + state.foundOf(zeroKey))


  ant = NodeBlockChain(genesis.hashNode,List(),RefundTransaction(twoKey,5),Array(0,0,0,0,0,0,0,0),2)
  if(state.receiveNode(ant)==0){
    System.out.println("NODO RECHAZADO")
  }
  System.out.println("Found of one: " + state.foundOf(oneKey))
  System.out.println("Found of zero: " + state.foundOf(zeroKey))
  System.out.println("dst res in node 1: " + state.diskNodes.readNode(1).refundTransaction.dstRest)
  state.close()

}
