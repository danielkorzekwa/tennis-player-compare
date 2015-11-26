package dk.tennis.compare.rating.multiskill.model.multipointcor

import scala.annotation.tailrec
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.bayes.math.linear.isIdentical

case class GenericMultiPointCorModel(p1PerfVariance: Double, p2PerfVariance: Double) extends MultiPointCorModel with LazyLogging {

  def skillMarginals(directSkills: DenseCanonicalGaussian, pointsWon: Int, allPoints: Int, maxIter: Int = 100, threshold: Double = 1e-5): DenseCanonicalGaussian = {

    val factorGraph = PointsFactorGraph(directSkills, p1PerfVariance, p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(currDirectSkills: DenseCanonicalGaussian, iterNum: Int): DenseCanonicalGaussian = {

      if (iterNum >= maxIter) logger.debug(s"Skills not converged in less than ${maxIter} iterations")

      factorGraph.sendMsgs()
      val skillsMarginal = factorGraph.getSkillsMarginal()

      if (iterNum >= maxIter || equals(skillsMarginal, currDirectSkills, threshold)) skillsMarginal else calibrate(skillsMarginal, iterNum + 1)
    }

    val skillsMarginal = calibrate(directSkills, 1)
    skillsMarginal
  }

  private def equals(gaussian1: DenseCanonicalGaussian, gaussian2: DenseCanonicalGaussian, tol: Double): Boolean =
    isIdentical(gaussian1.k, gaussian2.k, tol) &&
      isIdentical(gaussian1.h, gaussian2.h, tol) &&
      abs(gaussian1.g - gaussian2.g) < tol

}