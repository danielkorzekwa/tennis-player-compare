package dk.tennis.compare.rating.multiskill.model.multipoint

import scala.annotation.tailrec
import scala.math.abs
import dk.bayes.math.gaussian.Gaussian
import com.typesafe.scalalogging.slf4j.LazyLogging

case class GenericMultiPointModel(p1PerfVariance: Double, p2PerfVariance: Double) extends MultiPointModel with LazyLogging {

  def skillMarginals(player1Skill: Gaussian, player2Skill: Gaussian, pointsWon: Int, allPoints: Int, maxIter: Int = 100, threshold: Double = 1e-5): Tuple3[Gaussian, Gaussian, PointsFactorGraph] = {

    val factorGraph = PointsFactorGraph(player1Skill, player2Skill, p1PerfVariance, p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(p1Marginal: Gaussian, p2Marginal: Gaussian, iterNum: Int): Tuple2[Gaussian, Gaussian] = {

      if (iterNum >= maxIter) logger.debug(s"Skills not converged in less than ${maxIter} iterations")

      factorGraph.sendMsgs()
      val newP1Marginal = factorGraph.getP1Marginal()
      val newP2Marginal = factorGraph.getP2Marginal()

      if (iterNum >= maxIter || equals(newP1Marginal, p1Marginal, threshold) && equals(newP2Marginal, p2Marginal, threshold))
        (newP1Marginal, newP2Marginal)
      else calibrate(newP1Marginal, newP2Marginal, iterNum + 1)
    }

    val (p1Marginal, p2Marginal) = calibrate(player2Skill, player2Skill, 1)
    (p1Marginal, p2Marginal, factorGraph)

  }

  private def equals(gaussian1: Gaussian, gaussian2: Gaussian, threshold: Double): Boolean =
    abs(gaussian1.m - gaussian1.m) < threshold && (abs(gaussian1.v - gaussian2.v) < threshold)
}