package dk.tennis.compare.rating.multiskill.model.multipointcor

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

import scala.annotation.tailrec
import dk.bayes.math.gaussian.Gaussian
import scala.math._

case class GenericMultiPointCorModel(p1PerfVariance: Double, p2PerfVariance: Double) extends MultiPointCorModel {

  def skillMarginals(player1Skill: PlayerSkill, player2Skill: PlayerSkill, pointsWon: Int, allPoints: Int, skillCovariance: Double): Tuple3[PlayerSkill, PlayerSkill, Double] = {

    val p1SkillFactor = Gaussian(player1Skill.mean, player1Skill.variance)
    val p2SkillFactor = Gaussian(player2Skill.mean, player2Skill.variance)

    val threshold = 1e-5

    val factorGraph = PointsFactorGraph(p1SkillFactor, p2SkillFactor, skillCovariance, p1PerfVariance, p2PerfVariance, pointsWon, allPoints)

    @tailrec
    def calibrate(p1Marginal: Gaussian, p2Marginal: Gaussian, skillCovariance: Double): Tuple3[Gaussian, Gaussian, Double] = {
      factorGraph.sendMsgs()

      val newP1Marginal = factorGraph.getP1Marginal()
      val newP2Marginal = factorGraph.getP2Marginal()
      val newSkillCovariance = factorGraph.getCovariance()

      if (equals(newP1Marginal, p1Marginal, threshold) && equals(newP2Marginal, p2Marginal, threshold) && abs(newSkillCovariance - skillCovariance) < threshold)
        (newP1Marginal, newP2Marginal, newSkillCovariance)
      else calibrate(newP1Marginal, newP2Marginal, newSkillCovariance)
    }

    val (p1Marginal, p2Marginal, covarianceMarginal) = calibrate(p1SkillFactor, p2SkillFactor, skillCovariance)
    (PlayerSkill(p1Marginal.m, p1Marginal.v), PlayerSkill(p2Marginal.m, p2Marginal.v), covarianceMarginal)
  }

  private def equals(gaussian1: Gaussian, gaussian2: Gaussian, threshold: Double): Boolean =
    abs(gaussian1.m - gaussian1.m) < threshold && (abs(gaussian1.v - gaussian2.v) < threshold)
}