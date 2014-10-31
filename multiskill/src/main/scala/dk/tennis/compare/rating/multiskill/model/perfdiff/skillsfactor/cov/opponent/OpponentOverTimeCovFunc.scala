package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.util.Random

/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
class OpponentOverTimeCovFunc(val params: Seq[Double], val opponentCovFunc: OpponentCovFunc, val skillOverTimeCovFunc: SkillOverTimeCovFunc) extends CovFunc {

  def withParams(newParams: Seq[Double]): OpponentOverTimeCovFunc = {

    val Seq(opponentCovLogSf, opponentCovLogEll, logSfShort, logEllShort, logSfLong, logEllLong) = newParams

    val newOpponentCovFunc = opponentCovFunc.withParams(Array(opponentCovLogSf, opponentCovLogEll))
    val newSkillOverTimeCovFunc = skillOverTimeCovFunc.withParams(Array(logSfShort, logEllShort, logSfLong, logEllLong))

    new OpponentOverTimeCovFunc(newParams, newOpponentCovFunc, newSkillOverTimeCovFunc)
  }

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

  def opponentOnReturnSimMatrix(players: Seq[String]) = {
    opponentCovFunc.opponentOnReturnSimMatrix(players)
  }
}

object OpponentOverTimeCovFunc {
  def apply(params: Seq[Double],
    scores: Seq[Score], getPlayerSkill: (Player) => PlayerSkill): OpponentOverTimeCovFunc = {

    val Seq(
      opponentCovLogSf, opponentCovLogEll,
      logSfShort, logEllShort, logSfLong, logEllLong) = params

    val skillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player1), getPlayerSkill)
    val skillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player2), getPlayerSkill)

    val opponentCovFunc = OpponentCovFunc(Array(opponentCovLogSf, opponentCovLogEll), skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
    val skillOverTimeCovFunc = SkillOverTimeCovFunc(List(logSfShort, logEllShort, logSfLong, logEllLong))

    new OpponentOverTimeCovFunc(params, opponentCovFunc, skillOverTimeCovFunc)
  }

  /**
   * Returns Map[opponent name, player skills against opponent]
   */
  private def calcPriorSkillsGivenOpponent(playersGivenOpponent: Seq[Player], getPlayerSkill: (Player) => PlayerSkill): Map[String, Seq[PlayerSkill]] = {

    val allPlayers = playersGivenOpponent.map(p => p.playerName).distinct

    val skillsGivenOpponentMap = allPlayers.map { playerKey =>

      val skills = playersGivenOpponent.map { p =>
        val player = p.copy(opponentName = playerKey)
        val skill = getPlayerSkill(player)
        skill
      }.toSeq
      (playerKey, skills)
    }.toMap

    skillsGivenOpponentMap
  }
}