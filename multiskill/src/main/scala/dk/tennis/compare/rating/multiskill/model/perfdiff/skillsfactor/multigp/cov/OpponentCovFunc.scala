package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import scala.math._

/**
 *  @param params (
 * logSf - short term covariance -log of signal standard deviation
 * logEll - short term covariance - log of length scale standard deviation
 * logSf - long term covariance -log of signal standard deviation
 * logEll - long term covariance - log of length scale standard deviation
 * theSameOpponentLogSf -log of signal standard deviation
 * everyOpponentLogSf - log of length scale standard deviation
 */
case class OpponentCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(theSameOpponentLogSf, everyOpponentLogSf) = params

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    val theSameOpponentCov = if (player1.opponentName.equals(player2.opponentName)) exp(2 * theSameOpponentLogSf) else 0
    val everyOpponentCov = exp(2 * everyOpponentLogSf)
    val opponentCov = theSameOpponentCov + everyOpponentCov
    opponentCov
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val covD = paramIndex match {
      case 0 => cov_df_d0(player1, player2)
      case 1 => cov_df_d1(player1, player2)

    }
    covD
  }

  private def cov_df_d0(player1: Player, player2: Player): Double = {

    val theSameOpponentCovD = if (player1.opponentName.equals(player2.opponentName)) 2 * exp(2 * theSameOpponentLogSf) else 0

    theSameOpponentCovD + 0
  }

  private def cov_df_d1(player1: Player, player2: Player): Double = {
    0 + 2 * exp(2*everyOpponentLogSf)
  }

}