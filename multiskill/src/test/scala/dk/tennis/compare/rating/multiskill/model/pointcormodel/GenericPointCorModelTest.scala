package dk.tennis.compare.rating.multiskill.model.pointcormodel

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._

class GenericPointCorModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals_zero_skill_covariance {
    val skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0, 0, 0.5)))
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertEquals(Matrix(0.2914, -0.2654).toString, skillsMarginal.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.6908, 0.0065, 0.0065, 0.4953)).toString, skillsMarginal.variance.toString)

  }

  @Test def skillMarginals_positive_skill_covariance {

    val skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.3, 0.3, 0.5)))
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertEquals(Matrix(0.2527, -0.2263).toString, skillsMarginal.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.6969, 0.3015, 0.3015, 0.4992)).toString, skillsMarginal.variance.toString)

  }

  @Test def skillMarginals_till_convergence {

    var skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)))

    for (i <- 1 to 1000) {
      skills = pointModel.skillMarginals(skills, i % 5 == 0)
      println(pointModel.pointProb(skills))

    }

    assertEquals(Matrix(-2.6449, 1.8320).toString, skills.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.3116, 0.2773, 0.2773, 0.3018)).toString, skills.variance.toString)

  }

  @Test def skillsMarginals_zero_perf_variance {

    val perfVarianceOnServe = 1e-10
    val perfVarianceOnReturn = 1e-10
    val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

    val skills = CanonicalGaussian(Matrix(0.8, -0.8), Matrix(2, 2, Array(1, 0.2, 0.2, 1)))

    val skillsMarginal = pointModel.skillMarginals(skills, false)
    assertEquals(Matrix(-0.301, 0.301).toString, skillsMarginal.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.668, 0.532, 0.532, 0.668)).toString, skillsMarginal.variance.toString)
  }

  @Ignore @Test def skillsMarginals_NaN {

    val perfVarianceOnServe = 1
    val perfVarianceOnReturn = 1
    val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

    var skills = CanonicalGaussian(Matrix(2.592, 5.251), Matrix(2, 2, Array(-1d,0,0,-1)))

    println(pointModel.skillMarginals(skills, true).mean)
  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)))), 0.0001)
    assertEquals(0.528, pointModel.pointProb(CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.45, 0.45, 0.5)))), 0.0001)

    assertEquals(0.7268, pointModel.pointProb(CanonicalGaussian(Matrix(1.7, -1.8), Matrix(2, 2, Array(0.9, 0, 0, 0.8)))), 0.0001)
    assertEquals(0.7310, pointModel.pointProb(CanonicalGaussian(Matrix(1.7, -1.8), Matrix(2, 2, Array(0.9, 0.7, 0.7, 0.8)))), 0.0001)
  }
}