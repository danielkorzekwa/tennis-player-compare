package dk.tennis.compare.rating.multiskill.model.multipointcor

import org.junit.Assert._
import org.junit.Test
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.Matrix
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import dk.bayes.math.linear.isIdentical
import org.junit.Ignore

class GenericMultiPointCorModelTest {

  @Test def single_point_match_0_covariance {
    val directSkills = DenseCanonicalGaussian(DenseVector(0.2, -0.2), new DenseMatrix(2, 2, Array(0.7, 0d, 0d, 0.5)).t)

    val p1PerfVariance = 17
    val p2PerfVariance = 15

    val model = GenericMultiPointCorModel(p1PerfVariance, p2PerfVariance)
    val newDirectSkills = model.skillMarginals(directSkills, 3, 2)

    // println(new GenericPointCorModel(p1PerfVariance, p2PerfVariance).pointProb(newP1Skill, newP2Skill, newCovariance))

    assertTrue("actual=" + newDirectSkills.mean,isIdentical(DenseVector(0.3612, -0.3151), newDirectSkills.mean,0.0001))
     assertTrue("actual=" + newDirectSkills.variance,isIdentical(new DenseMatrix(2, 2, Array(0.6652, 0.0248, 0.0248, 0.4822)).t, newDirectSkills.variance,0.0001))
  }

  @Test def multiple_results_till_convergence {

    var directSkills = DenseCanonicalGaussian(DenseVector(-1, 0.5), new DenseMatrix(2, 2, Array(1, 0d, 0d, 1.2)).t)

    val p1PerfVariance = 190
    val p2PerfVariance = 170

    val model = GenericMultiPointCorModel(p1PerfVariance, p2PerfVariance)

    for (i <- 1 to 1000) {
      directSkills = model.skillMarginals(directSkills, 74, 100)

      println(new GenericPointCorModel(p1PerfVariance, p2PerfVariance).pointProb(directSkills))
    }

    assertTrue(isIdentical(DenseVector(5.210, -6.952), directSkills.mean,0.001))
     assertTrue(isIdentical(new DenseMatrix(2, 2, Array(0.5467, 0.5438, 0.5438, 0.5473)).t, directSkills.variance,0.001))

  }

  @Ignore @Test def check_perf_var_is_zero {
    val model = GenericMultiPointCorModel(p1PerfVariance = 2.075892613611445E-16, p2PerfVariance = 2.075892613611445E-16)

    val skills = DenseCanonicalGaussian(DenseVector(0d, 0), new DenseMatrix(2, 2, Array(1.1000198716370064, 0.0, 0.0, 1.1000080290094625)).t)
    val marginal = model.skillMarginals(skills, pointsWon = 25, allPoints = 42, maxIter = 49)

    println("marginal:" + marginal.mean + ":" + marginal.variance)
  }

  @Test def test {
    val model = GenericMultiPointCorModel(p1PerfVariance = 99.48431564193378, p2PerfVariance = 99.48431564193378)

    val skills = DenseCanonicalGaussian(DenseVector(2.121, 2.830), new DenseMatrix(2, 2, Array(15.688+3, 17.671, 17.671, 14.559+3)).t)
    var marginal = model.skillMarginals(skills, pointsWon = 31, allPoints = 62, maxIter = 49)

    println("marginal:" + marginal.mean + ":" + marginal.variance)
  }

}