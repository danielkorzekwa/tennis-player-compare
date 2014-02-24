package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import scala.annotation.tailrec
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.bayes.math.gaussian.CanonicalGaussian

case class GenericMultiPointCorModel(p1PerfVariance: Double, p2PerfVariance: Double) extends MultiPointCorModel {

  def skillMarginals(directSkills: CanonicalGaussian, pointsWon: Int, allPoints: Int): CanonicalGaussian = {

    val threshold = 1e-5

    val factorGraph = PointsFactorGraph(directSkills, p1PerfVariance, p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(currDirectSkills: CanonicalGaussian): CanonicalGaussian = {
      factorGraph.sendMsgs()

      val skillsMarginal = factorGraph.getSkillsMarginal()

      if (equals(skillsMarginal, currDirectSkills, threshold)) skillsMarginal else calibrate(skillsMarginal)
    }

    val skillsMarginal = calibrate(directSkills)
    skillsMarginal
  }

  private def equals(gaussian1: CanonicalGaussian, gaussian2: CanonicalGaussian, threshold: Double): Boolean =
    gaussian1.k.matrix.isIdentical(gaussian2.k.matrix, threshold) &&
      gaussian1.h.matrix.isIdentical(gaussian2.h.matrix, threshold) &&
      abs(gaussian1.g - gaussian2.g) < threshold
}