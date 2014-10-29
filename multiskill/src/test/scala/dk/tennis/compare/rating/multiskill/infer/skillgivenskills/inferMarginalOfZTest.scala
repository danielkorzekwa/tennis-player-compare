package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import org.junit._
import Assert._
import dk.bayes.math.linear.Matrix

class inferMarginalOfZTest {

  @Test def test {
    val xPriorMean = Matrix(0, 0, 0)
    val xPriorVarInv = Matrix(3, 3, Array(1.090, 1.053, 1.089, 1.053, 1.090, 1.061, 1.089, 1.061, 1.090)).inv

    val xMean = Matrix(0.200, 0.800, -0.100)
    val Kxx = Matrix(3, 3, Array(1.090, 1.053, 1.089, 1.053, 1.090, 1.061, 1.089, 1.061, 1.090))

    val zPriorMean = Matrix(0)
    val Kzz = Matrix(1.090)
    val Kzx = Matrix(0.1, 1, 0.1).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)
    assertEquals(0.1950, playerSkill.m, 0.0001)
    assertEquals(1.0900, playerSkill.v, 0.0001)
  }

  @Test def test2 {
    val xPriorMean = Matrix(5.000, 5.000, 5.000, 5.000, 5.000, 5.000)

    val xPriorVarInv = Matrix(6, 6, Array(
      4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000, 0.000,
      0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000,
      0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090,
      4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000,
      0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000,
      0.000, 0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10)).inv

    val xMean = Matrix(2.550, 4.892, 4.892, 2.550, 4.892, 4.892)
    val Kxx = Matrix(6, 6, Array(
      2.151 + 1e-10, 0.000, 0.000, 2.151, 0.000, 0.000,
      0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184, 0.000,
      0.000, 0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184,
      2.151, 0.000, 0.000, 2.151 + 1e-10, 0.000, 0.000,
      0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10, 0.000,
      0.000, 0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10))

    val zPriorMean = Matrix(5.000)
    val Kzz = Matrix(4.090)
    val Kzx = Matrix(4.090, 0.000, 0.000, 4.090, 0.000, 0.000).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)

    assertEquals(2.55, playerSkill.m, 0.0001)
    assertEquals(2.151, playerSkill.v, 0.0001)
  }

  @Test def test3 {
    val xPriorMean = Matrix(5.000, 5.000, 5.000, 5.000, 5.000, 5.000)

    val xPriorVarInv = Matrix(6, 6, Array(
      4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000, 0.000,
      0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090, 0.000,
      0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000, 4.090,
      4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000, 0.000,
      0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10, 0.000,
      0.000, 0.000, 4.090, 0.000, 0.000, 4.090 + 1e-10)).inv

    val xMean = Matrix(2.550, 4.892, 4.892, 2.550, 4.892, 4.892)
    val Kxx = Matrix(6, 6, Array(
      2.151 + 1e-10, 0.000, 0.000, 2.151, 0.000, 0.000,
      0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184, 0.000,
      0.000, 0.000, 2.184 + 1e-10, 0.000, 0.000, 2.184,
      2.151, 0.000, 0.000, 2.151 + 1e-10, 0.000, 0.000,
      0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10, 0.000,
      0.000, 0.000, 2.184, 0.000, 0.000, 2.184 + 1e-10))

    val zPriorMean = Matrix(5)
    val Kzz = Matrix(4.090)
    val Kzx = Matrix(0.090, 0.000, 0.000, 0.090, 0.000, 0.000).t

    val playerSkill = inferMarginalOfZ(xPriorMean, xPriorVarInv, xMean, Kxx, zPriorMean, Kzz, Kzx)

    assertEquals(4.9460, playerSkill.m, 0.0001)
    assertEquals(4.0890, playerSkill.v, 0.0001)
  }

}