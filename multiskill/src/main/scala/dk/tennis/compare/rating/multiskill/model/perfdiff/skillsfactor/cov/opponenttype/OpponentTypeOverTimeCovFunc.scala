package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

case class OpponentTypeOverTimeCovFunc(params: Seq[Double], opponentTypeMap: Map[String, OpponentType]) extends CovFunc {

  private val Seq(
    generalSkillLogSf, offensiveSkillLogSf, defensiveSkillLogSf,
    logSfShort, logEllShort, logSfLong, logEllLong) = params

  private val opponentTypeCovFunc = OpponentTypeCovFunc(List(generalSkillLogSf, offensiveSkillLogSf, defensiveSkillLogSf), opponentTypeMap)
  private val skillOverTimeCovFunc = SkillOverTimeCovFunc(List(logSfShort, logEllShort, logSfLong, logEllLong))

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")

  def withParams(newParams: Seq[Double]): OpponentTypeOverTimeCovFunc = {
    OpponentTypeOverTimeCovFunc(newParams, opponentTypeMap)
  }

  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    val opponentTypeCov = opponentTypeCovFunc.covariance(player1, player2)
    val skillOverTimeCov = skillOverTimeCovFunc.covariance(player1, player2)

    if (player1.onServe) opponentTypeCov * skillOverTimeCov
    else opponentTypeCov * skillOverTimeCov
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {
    val covD = paramIndex match {
      case 0 => opponentTypeCovFunc.covarianceD(player1, player2, 0) * skillOverTimeCovFunc.covariance(player1, player2)
      case 1 => opponentTypeCovFunc.covarianceD(player1, player2, 1) * skillOverTimeCovFunc.covariance(player1, player2)
      case 2 => opponentTypeCovFunc.covarianceD(player1, player2, 2) * skillOverTimeCovFunc.covariance(player1, player2)
      case 3 => opponentTypeCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 0)
      case 4 => opponentTypeCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 1)
      case 5 => opponentTypeCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 2)
      case 6 => opponentTypeCovFunc.covariance(player1, player2) * skillOverTimeCovFunc.covarianceD(player1, player2, 3)
    }

    covD
  }

}