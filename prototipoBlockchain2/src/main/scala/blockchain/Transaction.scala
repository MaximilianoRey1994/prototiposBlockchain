package blockchain

case class Transaction(inputs:List[SignedUTXO],outputs:List[UTXO]){
  lazy val mountToMiner = {
    val inputV = inputs.foldRight(0:Long)((x,rec) => x.utxo.value + rec)
    val outputV = outputs.foldRight(0:Long)((x,rec) => x.value + rec)
    inputV - outputV
  }
  def verify():Boolean = {
    System.out.println("MOUNT TO MINER: " + this.mountToMiner)
    (this.mountToMiner>=0) && inputs.foldRight(true)((x,rec) => x.verify(outputs) && rec)
  }
}
