package dk.tennis.compare.rating.multiskill.model.pointcormodel

import org.junit.Assert._
import org.junit.Ignore
import org.junit.Test
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.bayes.math.linear.isIdentical

class GenericPointCorModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals_zero_skill_covariance {
    val skills = DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0, 0, 0.5)).t)
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertTrue(isIdentical(DenseVector(0.2914, -0.2654), skillsMarginal.mean,0.0001))
    assertTrue(isIdentical(new DenseMatrix(2, 2, Array(0.6908, 0.0065, 0.0065, 0.4953)).t, skillsMarginal.variance,0.0001))

  }

  @Test def skillMarginals_positive_skill_covariance {

    val skills = DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0.3, 0.3, 0.5)).t)
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertTrue("actual=" + skillsMarginal.mean,isIdentical(DenseVector(0.2527, -0.2263), skillsMarginal.mean,0.001))
    assertTrue(isIdentical(new DenseMatrix(2, 2, Array(0.6969, 0.3015, 0.3015, 0.4992)).t, skillsMarginal.variance,0.0001))

  }

  @Test def skillMarginals_till_convergence {

    var skills = DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)).t)

    for (i <- 1 to 1000) {
      skills = pointModel.skillMarginals(skills, i % 5 == 0)
      println(pointModel.pointProb(skills))

    }

     assertTrue("actual=" + skills.mean,isIdentical(DenseVector(-2.642, 1.830), skills.mean,0.001))
     assertTrue(isIdentical(new DenseMatrix(2, 2, Array(0.3116, 0.2773, 0.2773, 0.3018)).t, skills.variance,0.001))

  }

  @Test def skillsMarginals_zero_perf_variance {

    val perfVarianceOnServe = 1e-10
    val perfVarianceOnReturn = 1e-10
    val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

    val skills = DenseCanonicalGaussian(DenseVector(0.8, -0.8), new DenseMatrix(2, 2, Array(1, 0.2, 0.2, 1)).t)

    val skillsMarginal = pointModel.skillMarginals(skills, false)
     assertTrue(isIdentical(DenseVector(-0.301, 0.301), skillsMarginal.mean,0.001))
     assertTrue(isIdentical(new DenseMatrix(2, 2, Array(0.668, 0.532, 0.532, 0.668)).t, skillsMarginal.variance,0.001))
  }

  @Ignore @Test def skillsMarginals_NaN {

    val perfVarianceOnServe = 1
    val perfVarianceOnReturn = 1
    val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

    var skills = DenseCanonicalGaussian(DenseVector(2.592, 5.251), new DenseMatrix(2, 2, Array(-1d,0,0,-1)).t)

    println(pointModel.skillMarginals(skills, true).mean)
  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)).t)), 0.0001)
    assertEquals(0.528, pointModel.pointProb(DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0.45, 0.45, 0.5)).t)), 0.0001)

    assertEquals(0.7268, pointModel.pointProb(DenseCanonicalGaussian(DenseVector(1.7, -1.8), new DenseMatrix(2, 2, Array(0.9, 0, 0, 0.8)).t)), 0.0001)
    assertEquals(0.7310, pointModel.pointProb(DenseCanonicalGaussian(DenseVector(1.7, -1.8), new DenseMatrix(2, 2, Array(0.9, 0.7, 0.7, 0.8)).t)), 0.0001)
  }
}