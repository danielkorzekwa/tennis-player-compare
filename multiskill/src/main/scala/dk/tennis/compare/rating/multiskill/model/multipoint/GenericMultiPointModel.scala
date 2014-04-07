package dk.tennis.compare.rating.multiskill.model.multipoint

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import scala.annotation.tailrec
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.bayes.model.factor.BivariateGaussianFactor

case class GenericMultiPointModel(p1PerfVariance: Double,p2PerfVariance: Double) extends MultiPointModel {

  def skillMarginals(player1Skill: Gaussian, player2Skill: Gaussian, pointsWon: Int, allPoints: Int): Tuple3[Gaussian,Gaussian,PointsFactorGraph] = {

    val threshold = 1e-5

    val factorGraph = PointsFactorGraph(player1Skill, player2Skill, p1PerfVariance,p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(p1Marginal: Gaussian, p2Marginal: Gaussian): Tuple2[Gaussian, Gaussian] = {
      factorGraph.sendMsgs()

      val newP1Marginal = factorGraph.getP1Marginal()
      val newP2Marginal = factorGraph.getP2Marginal()

      if (equals(newP1Marginal, p1Marginal, threshold) && equals(newP2Marginal, p2Marginal, threshold))
        (newP1Marginal, newP2Marginal)
      else calibrate(newP1Marginal, newP2Marginal)
    }

    val (p1Marginal, p2Marginal) = calibrate(player2Skill, player2Skill)
  (p1Marginal,p2Marginal,factorGraph)

  }
  
  private def equals(gaussian1: Gaussian, gaussian2: Gaussian, threshold: Double): Boolean =
    abs(gaussian1.m - gaussian1.m) < threshold && (abs(gaussian1.v - gaussian2.v) < threshold)
}