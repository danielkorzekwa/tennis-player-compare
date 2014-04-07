package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import scala.annotation.tailrec
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.bayes.math.gaussian.CanonicalGaussian
import com.typesafe.scalalogging.slf4j.Logging

case class GenericMultiPointCorModel(p1PerfVariance: Double, p2PerfVariance: Double) extends MultiPointCorModel with Logging {

  def skillMarginals(directSkills: CanonicalGaussian, pointsWon: Int, allPoints: Int,maxIter:Int=100,threshold:Double = 1e-5): CanonicalGaussian = {

    val factorGraph = PointsFactorGraph(directSkills, p1PerfVariance, p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(currDirectSkills: CanonicalGaussian,iterNum:Int): CanonicalGaussian = {
      require(iterNum<=maxIter,s"Skills not converged in less than ${maxIter} iterations")
      factorGraph.sendMsgs()

      val skillsMarginal = factorGraph.getSkillsMarginal()

      if (equals(skillsMarginal, currDirectSkills, threshold)) skillsMarginal else calibrate(skillsMarginal,iterNum+1)
    }

    val skillsMarginal = calibrate(directSkills,1)
    skillsMarginal
  }

  private def equals(gaussian1: CanonicalGaussian, gaussian2: CanonicalGaussian, threshold: Double): Boolean =
    gaussian1.k.matrix.isIdentical(gaussian2.k.matrix, threshold) &&
      gaussian1.h.matrix.isIdentical(gaussian2.h.matrix, threshold) &&
      abs(gaussian1.g - gaussian2.g) < threshold
}