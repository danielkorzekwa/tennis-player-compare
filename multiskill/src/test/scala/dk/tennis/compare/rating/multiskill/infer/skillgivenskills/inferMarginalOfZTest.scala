package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import org.junit._
import Assert._
import breeze.linalg.Matrix
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg.inv

class inferMarginalOfZTest {

  @Test def test {
    val xPriorMean = DenseVector(0.2, 0.8, 0.6)
    val xPriorVarInv = inv(new DenseMatrix(3, 3, Array(1.090, 1.053, 1.089, 1.053, 1.090, 1.061, 1.089, 1.061, 1.090)).t)

    val xMean = DenseVector(0.2, 0.8, 0.6)
    val Kxx = new DenseMatrix(3, 3, Array(1.090, 1.053, 1.089, 1.053, 1.090, 1.061, 1.089, 1.061, 1.090)).t

    val zPriorMean = DenseVector(0.6)
    val Kzz = DenseMatrix(1.090)
    val Kzx = DenseMatrix( 1.053, 1.090, 1.061).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)
    assertEquals(0.6, playerSkill.m, 0.0001)
    assertEquals(1.0900, playerSkill.v, 0.0001)
  }

  @Test def test2 {
    val xPriorMean = DenseVector(5.000, 5.000, 5.000, 5.000, 5.000, 5.000)

    val xPriorVarInv = inv(new DenseMatrix(6, 6, Array(
      4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000, 0.000,
      0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000,
      0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090,
      4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000,
      0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000,
      0.000, 0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10)).t)

    val xMean = DenseVector(2.550, 4.892, 4.892, 2.550, 4.892, 4.892)
    val Kxx = new DenseMatrix(6, 6, Array(
      2.151 + 1e-10, 0.000, 0.000, 2.151, 0.000, 0.000,
      0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184, 0.000,
      0.000, 0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184,
      2.151, 0.000, 0.000, 2.151 + 1e-10, 0.000, 0.000,
      0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10, 0.000,
      0.000, 0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10)).t

    val zPriorMean = DenseVector(5.000)
    val Kzz = DenseMatrix(4.090)
    val Kzx = DenseMatrix(4.090, 0.000, 0.000, 4.090, 0.000, 0.000).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)

    assertEquals(2.55, playerSkill.m, 0.0001)
    assertEquals(2.151, playerSkill.v, 0.0001)
  }

  @Test def test3 {
    val xPriorMean = DenseVector(5.000, 5.000, 5.000, 5.000, 5.000, 5.000)

    val xPriorVarInv = inv(new DenseMatrix(6, 6, Array(
      4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000, 0.000,
      0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000,
      0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090,
      4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000,
      0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000,
      0.000, 0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10)).t)

    val xMean = DenseVector(2.550, 4.892, 4.892, 2.550, 4.892, 4.892)
    val Kxx = new DenseMatrix(6, 6, Array(
      2.151 + 1e-10, 0.000, 0.000, 2.151, 0.000, 0.000,
      0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184, 0.000,
      0.000, 0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184,
      2.151, 0.000, 0.000, 2.151 + 1e-10, 0.000, 0.000,
      0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10, 0.000,
      0.000, 0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10)).t

    val zPriorMean = DenseVector(5d)
    val Kzz = DenseMatrix(4.090)
    val Kzx = DenseMatrix(0.090, 0.000, 0.000, 0.090, 0.000, 0.000).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)

    assertEquals(4.9460, playerSkill.m, 0.0001)
    assertEquals(4.0890, playerSkill.v, 0.0001)
  }

}