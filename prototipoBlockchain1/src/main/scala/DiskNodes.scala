import java.io.{File, RandomAccessFile}

import java.nio.ByteBuffer

class DiskNodes(file:String) {
  val randomAccessFile:RandomAccessFile = new RandomAccessFile(new File(file),"rw")
  def recuperateDir(i:Long):Long = (Extras.maxNodeLength + 4)*i
  def readNode(i:Long): NodeBlockChain = {
    randomAccessFile.seek(recuperateDir(i))
    val arr:Array[Byte] = Array.fill[Byte](Extras.maxNodeLength+4)(0)
    randomAccessFile.read(arr)
    NodeCodecContract.decodeImpl(0, ByteBuffer.wrap(arr)).right.get.decoded
  }
  def writeNode(i:Long,n:NodeBlockChain): Unit ={
    randomAccessFile.seek(recuperateDir(i))
    val arr = ByteBuffer.allocate(Extras.maxNodeLength+4)
    NodeCodecContract.encodeImpl(n,0,arr)
    randomAccessFile.write(arr.array())
  }
  def close():Unit = randomAccessFile.close()
}
