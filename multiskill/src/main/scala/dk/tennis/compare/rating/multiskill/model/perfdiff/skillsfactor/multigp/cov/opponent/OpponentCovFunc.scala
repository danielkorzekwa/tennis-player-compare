package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

/**
 * Inspired by
 *  'Kohei Hayashi, Takashi Takenouchi . Self-measuring Similarity for Multi-task Gaussian Process. 2012'
 *  http://jmlr.org/proceedings/papers/v27/hayashi12a/hayashi12a.pdf
 *
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
case class OpponentCovFunc(params: Seq[Double],
  skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]) extends CovFunc {

  private val Seq(opponentCovLogSf, opponentCovLogEll) = params

  private val opponentCovFunc = new CovSEiso(sf = opponentCovLogSf, opponentCovLogEll)

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    require(player1.onServe == player2.onServe)

    val (skillsGivenOpponent1, skillsGivenOpponent2) = skillsGivenOpponent(player1, player2)
    val opponentCovVal = opponentCovFunc.cov(skillsGivenOpponent1, skillsGivenOpponent2)
    opponentCovVal
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val (skillsGivenOpponent1, skillsGivenOpponent2) = skillsGivenOpponent(player1, player2)

    val covD = paramIndex match {

      case 0 => opponentCovFunc.df_dSf(skillsGivenOpponent1, skillsGivenOpponent2)
      case 1 => opponentCovFunc.df_dEll(skillsGivenOpponent1, skillsGivenOpponent2)

    }

    covD
  }

  private def skillsGivenOpponent(player1: Player, player2: Player): Tuple2[Matrix, Matrix] = {
    require(player1.onServe == player2.onServe)

    val (skillsGivenOpponent1, skillsGivenOpponent2) = if (player1.onServe) {
      val skillsGivenOpponent1 = Matrix(skillsOnReturnGivenOpponent(player1.opponentName).map(skill => skill.skill).toArray)
      val skillsGivenOpponent2 = Matrix(skillsOnReturnGivenOpponent(player2.opponentName).map(skill => skill.skill).toArray)
      (skillsGivenOpponent1, skillsGivenOpponent2)
    } else {
      val skillsGivenOpponent1 = Matrix(skillsOnServeGivenOpponent(player1.opponentName).map(skill => skill.skill).toArray)
      val skillsGivenOpponent2 = Matrix(skillsOnServeGivenOpponent(player2.opponentName).map(skill => skill.skill).toArray)
      (skillsGivenOpponent1, skillsGivenOpponent2)
    }

    (skillsGivenOpponent1, skillsGivenOpponent2)
  }
}