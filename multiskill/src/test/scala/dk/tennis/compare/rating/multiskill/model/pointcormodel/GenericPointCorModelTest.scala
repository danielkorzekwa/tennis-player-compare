package dk.tennis.compare.rating.multiskill.model.pointcormodel

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear._

class GenericPointCorModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals_zero_skill_covariance {
    val skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0, 0, 0.5)))
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertEquals(Matrix(0.2914, -0.2654).toString, skillsMarginal.getMean.toString)
    assertEquals(Matrix(2, 2, Array(0.6908, 0.0065, 0.0065, 0.4953)).toString, skillsMarginal.getVariance.toString)

  }

  @Test def skillMarginals_positive_skill_covariance {

    val skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.3, 0.3, 0.5)))
    val skillsMarginal = pointModel.skillMarginals(skills, true)

    assertEquals(Matrix(0.2527, -0.2263).toString, skillsMarginal.getMean.toString)
    assertEquals(Matrix(2, 2, Array(0.6969, 0.3015, 0.3015, 0.4992)).toString, skillsMarginal.getVariance.toString)

  }

  @Test def skillMarginals_till_convergence {

    var skills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)))

    for (i <- 1 to 1000) {
      skills = pointModel.skillMarginals(skills, i % 5 == 0)
      println(pointModel.pointProb(skills))

    }

    assertEquals(Matrix(-2.6449, 1.8320).toString, skills.getMean.toString)
    assertEquals(Matrix(2, 2, Array(0.3116, 0.2773, 0.2773, 0.3018)).toString, skills.getVariance.toString)

  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.0, 0.0, 0.5)))), 0.0001)
    assertEquals(0.528, pointModel.pointProb(CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0.45, 0.45, 0.5)))), 0.0001)

    assertEquals(0.7268, pointModel.pointProb(CanonicalGaussian(Matrix(1.7, -1.8), Matrix(2, 2, Array(0.9, 0, 0, 0.8)))), 0.0001)
    assertEquals(0.7310, pointModel.pointProb(CanonicalGaussian(Matrix(1.7, -1.8), Matrix(2, 2, Array(0.9, 0.7, 0.7, 0.8)))), 0.0001)
  }
}