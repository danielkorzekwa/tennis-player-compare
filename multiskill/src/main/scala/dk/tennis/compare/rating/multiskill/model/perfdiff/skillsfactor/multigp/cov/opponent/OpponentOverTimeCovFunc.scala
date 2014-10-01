package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.skillovertime.SkillOverTimeCovFunc
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix

/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
case class OpponentOverTimeCovFunc(params: Seq[Double], skillsGivenOpponent: Map[String, Seq[PlayerSkill]]) extends CovFunc {

  private val Seq(
    opponentCovLogSf, opponentCovLogEll,
    logSfShort, logEllShort, logSfLong, logEllLong) = params

  private val opponentCovFunc = OpponentCovFunc(Array(opponentCovLogSf, opponentCovLogEll), skillsGivenOpponent)
  private val skillOverTimeCovFunc = SkillOverTimeCovFunc(List(logSfShort, logEllShort, logSfLong, logEllLong))

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    val opponentCovVal = opponentCovFunc.covariance(player1, player2)
    val skillOverTimeCovVal = skillOverTimeCovFunc.covariance(player1, player2)
    opponentCovVal * skillOverTimeCovVal
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {
    val covD = paramIndex match {

      case 0 => opponentCovFunc.covarianceD(player1, player2, 0) * skillOverTimeCovFunc.covariance(player1, player2)
      case 1 => opponentCovFunc.covarianceD(player1, player2, 1) * skillOverTimeCovFunc.covariance(player1, player2)
      case 2 => opponentCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 0)
      case 3 => opponentCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 1)
      case 4 => opponentCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 2)
      case 5 => opponentCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 3)
    }

    covD
  }
}