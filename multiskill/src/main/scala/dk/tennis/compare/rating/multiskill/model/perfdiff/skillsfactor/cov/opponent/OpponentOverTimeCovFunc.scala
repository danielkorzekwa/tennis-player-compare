package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.covfunc.CovSEiso
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.util.Random
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream

/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
class OpponentOverTimeCovFunc(val params: Seq[Double], val scores: Seq[Score], val opponentCovFunc: OpponentCovFunc, val skillOverTimeCovFunc: SkillOverTimeCovFunc) extends CovFunc with Serializable {

  def withParams(newParams: Seq[Double]): OpponentOverTimeCovFunc = {

    val Seq(opponentCovLogSf, opponentCovLogEll, logSfShort, logEllShort, logSfLong, logEllLong) = newParams

    val newOpponentCovFunc = opponentCovFunc.withParams(Array(opponentCovLogSf, opponentCovLogEll))
    val newSkillOverTimeCovFunc = skillOverTimeCovFunc.withParams(Array(logSfShort, logEllShort, logSfLong, logEllLong))

    new OpponentOverTimeCovFunc(newParams, scores, newOpponentCovFunc, newSkillOverTimeCovFunc)
  }

  def save(file: String) = {
    new ObjectOutputStream(new FileOutputStream(file)).writeObject(this)
  }

  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): OpponentOverTimeCovFunc = {

    OpponentOverTimeCovFunc(params, scores, getPlayerSkill)
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

  def fromFile(file: String): OpponentOverTimeCovFunc = {
    new ObjectInputStream(new FileInputStream(file)).readObject().asInstanceOf[OpponentOverTimeCovFunc]
  }

  def apply(params: Seq[Double],
    scores: Seq[Score], getPlayerSkill: (Player) => PlayerSkill): OpponentOverTimeCovFunc = {

    val Seq(
      opponentCovLogSf, opponentCovLogEll,
      logSfShort, logEllShort, logSfLong, logEllLong) = params

    val opponentCovFunc = OpponentCovFunc(Array(opponentCovLogSf, opponentCovLogEll), scores, getPlayerSkill)
    val skillOverTimeCovFunc = SkillOverTimeCovFunc(List(logSfShort, logEllShort, logSfLong, logEllLong))

    new OpponentOverTimeCovFunc(params, scores, opponentCovFunc, skillOverTimeCovFunc)
  }

}
