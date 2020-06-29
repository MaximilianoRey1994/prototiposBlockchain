package blockchain

import java.io.{File, IOError, RandomAccessFile}

import librarys.codec.BlockCodec
import scodec.bits.BitVector

class DiskMap(path: String) {
  def get(i:Long): Block ={
    val file = new RandomAccessFile(new File(path + (i/1000).toString),"r")
    file.seek((i % 1000) * 12)
    try {
      val pos = file.readLong()
      val size = file.readInt()
      if(pos==0) return null
      file.seek(pos)
      val encoded = new Array[Byte](size)
      file.read(encoded)
      BlockCodec.decode(BitVector(encoded)).require.value
    }catch{
      case x:IOError => null
    }
  }
  def set(i:Long,block:Block): Unit ={
    val encoded = BlockCodec.encode(block).require.toByteArray
    val file = new RandomAccessFile(new File(path + (i/1000).toString),"rw")
    val pos = file.length()
    file.seek((i % 1000) * 12);
    file.writeLong(pos)
    file.writeInt(encoded.length)
    file.seek(pos)
    file.write(encoded)
  }
}
