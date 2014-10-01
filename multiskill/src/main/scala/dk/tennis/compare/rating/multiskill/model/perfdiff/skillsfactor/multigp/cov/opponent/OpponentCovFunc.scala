package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
case class OpponentCovFunc(params: Seq[Double], skillsGivenOpponent: Map[String, Seq[PlayerSkill]]) extends CovFunc {

  private val Seq(opponentCovLogSf, opponentCovLogEll) = params

  private val opponentCovFunc = new CovSEiso(sf = opponentCovLogSf, opponentCovLogEll)

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    val skillsGivenOpponent1 = Matrix(skillsGivenOpponent(player1.opponentName).map(skill => skill.skill).toArray)
    val skillsGivenOpponent2 = Matrix(skillsGivenOpponent(player2.opponentName).map(skill => skill.skill).toArray)

    val opponentCovVal = opponentCovFunc.cov(skillsGivenOpponent1, skillsGivenOpponent2)
    opponentCovVal
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val skillsGivenOpponent1 = Matrix(skillsGivenOpponent(player1.opponentName).map(skill => skill.skill).toArray)
    val skillsGivenOpponent2 = Matrix(skillsGivenOpponent(player2.opponentName).map(skill => skill.skill).toArray)

    val covD = paramIndex match {

      case 0 => opponentCovFunc.df_dSf(skillsGivenOpponent1, skillsGivenOpponent2)
      case 1 => opponentCovFunc.df_dEll(skillsGivenOpponent1, skillsGivenOpponent2)

    }

    covD
  }
}