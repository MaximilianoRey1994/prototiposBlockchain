package librarys.crypto

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint

object Utils {
  def ensamblateBlocks(results: Seq[Array[Byte]], numberLength: Byte): Array[Byte] = {
    val res = new Array[Byte](numberLength * (results.length - 1) + results.last(0))
    var h = 0
    for (i <- 0 until (results.length - 1)) {
      if (results(i)(0) != numberLength.toByte) throw new RuntimeException("PROBLEM!!!!!")
      for (j <- 1 to numberLength) {
        res(h) = results(i)(j)
        h = h + 1
      }
    }
    for (j <- 1 to results.last(0)) {
      res(h) = results.last(j)
      h = h + 1
    }
    res
  }

  def divideInBlocks(data: Array[Byte], numberLength: Byte): Array[Array[Byte]] = {
    val lengthR = if (data.length % numberLength == 0) data.length / numberLength else data.length / numberLength + 1
    val res = new Array[Array[Byte]](lengthR)
    var h = 0
    for (i <- 0 until data.length / numberLength) {
      res(i) = new Array[Byte](numberLength + 1)
      res(i)(0) = numberLength.toByte
      for (j <- 1 to numberLength) {
        res(i)(j) = data(h)
        h = h + 1
      }
    }
    if (data.length % numberLength != 0) {
      res(data.length / numberLength) = new Array[Byte](numberLength + 1)
      res.last(0) = (data.length % numberLength).toByte
      for (j <- 1 until (data.length % numberLength + 1)) {
        res.last(j) = data(h)
        h = h + 1
      }
      for (j <- (data.length % numberLength + 1) to numberLength) {
        res.last(j) = 0
      }
    }
    res
  }

  def calculatePoint(n: BigInteger): ECPoint = {
    val nFieldElement = curve.getCurve.fromBigInteger(n)
    val tmp = nFieldElement.multiply(nFieldElement).multiply(nFieldElement).add(curve.getCurve.getB)
    val xFieldElement = tmp.multiply(tmp)
    val yFieldElement = xFieldElement.multiply(xFieldElement)
    val res = curve.getCurve.createPoint(nFieldElement.toBigInteger, yFieldElement.toBigInteger)
    if (!res.isValid) throw new RuntimeException("INVALID POINT")
    res
    // y = sqrt((x*x+a)*x + b)

    //y*y  = n*n*n + 7

    // y = n*n
    //x = n*n*n + 7
    //x = 7n
    // y2 = (n3 + a) * (n3 + a )

    // (x * x * x + 7) = (n3 +a)2 = n6 + an3 + a2
    //x3 = n6 + an3 + a2 -7
    //x3 = n3 (n3+a) + a2-7
  }
}
