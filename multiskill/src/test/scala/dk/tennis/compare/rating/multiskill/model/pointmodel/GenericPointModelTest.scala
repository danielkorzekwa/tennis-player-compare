package dk.tennis.compare.rating.multiskill.model.pointmodel

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import dk.bayes.math.gaussian.Gaussian

class GenericPointModelTest {

  private val perfVarianceOnServe = 17
  private val perfVarianceOnReturn = 15
  private val pointModel = GenericPointModel(perfVarianceOnServe, perfVarianceOnReturn)

  @Test def skillMarginals {

    val (p1Marginal, p2Marginal) = pointModel.skillMarginals(Gaussian(0.2, 0.7), Gaussian(-0.2, 0.5), true)
    assertGaussian(Gaussian(0.2916, 0.6908), p1Marginal, 0.0001)
    assertGaussian(Gaussian(-0.2654, 0.4953), p2Marginal, 0.0001)

    println(pointModel.perfMarginals(Gaussian(0.2, 0.7), Gaussian(-0.2, 0.5), true)._1)
  }

  @Test def skillMarginals_till_convergence {

    var p1Skill = Gaussian(0.2, 0.7)
    var p2Skill = Gaussian(-0.2, 0.5)

    for (i <- 1 to 1000) {
      val (p1Marginal, p2Marginal) = pointModel.skillMarginals(p1Skill, p2Skill, i % 5==0)
      p1Skill = p1Marginal
      p2Skill = p2Marginal
      
     println( pointModel.pointProb(p1Skill,p2Skill))

    }
    
    assertGaussian(Gaussian(-2.6371, 0.0581), p1Skill, 0.0001)
    assertGaussian(Gaussian(2.0652, 0.0563), p2Skill, 0.0001)

  }

  @Test def pointProb {
    assertEquals(0.5276, pointModel.pointProb(Gaussian(0.2, 0.7), Gaussian(-0.2, 0.5)), 0.0001)
  }
}