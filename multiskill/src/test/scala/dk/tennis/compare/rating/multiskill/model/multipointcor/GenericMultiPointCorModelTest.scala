package dk.tennis.compare.rating.multiskill.model.multipointcor

import org.scalatest.Matchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import org.junit._
import Assert._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._

class GenericMultiPointCorModelTest {

  @Test def single_point_match_0_covariance {
    val directSkills = CanonicalGaussian(Matrix(0.2, -0.2), Matrix(2, 2, Array(0.7, 0d, 0d, 0.5)))

    val p1PerfVariance = 17
    val p2PerfVariance = 15

    val model = GenericMultiPointCorModel(p1PerfVariance, p2PerfVariance)
    val newDirectSkills = model.skillMarginals(directSkills, 1, 1)

    // println(new GenericPointCorModel(p1PerfVariance, p2PerfVariance).pointProb(newP1Skill, newP2Skill, newCovariance))

    assertEquals(Matrix(0.291, -0.265).toString, newDirectSkills.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.691, 0.0065, 0.0065, 0.4953)).toString, newDirectSkills.variance.toString)
  }

  @Test def multiple_results_till_convergence {

    var directSkills = CanonicalGaussian(Matrix(-1, 0.5), Matrix(2, 2, Array(1, 0d, 0d, 1.2)))

    val p1PerfVariance = 190
    val p2PerfVariance = 170

    val model = GenericMultiPointCorModel(p1PerfVariance, p2PerfVariance)

    for (i <- 1 to 1000) {
      directSkills = model.skillMarginals(directSkills, 74, 100)

      println(new GenericPointCorModel(p1PerfVariance, p2PerfVariance).pointProb(directSkills))
    }

    assertEquals(Matrix(4.9887, -6.6864).toString, directSkills.mean.toString)
    assertEquals(Matrix(2, 2, Array(0.5467, 0.5438, 0.5438, 0.5473)).toString, directSkills.variance.toString)

  }

}