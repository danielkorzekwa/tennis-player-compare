package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponentseiso

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import breeze.linalg.DenseMatrix

case class OpponentSeIsoCovFunc(params: Seq[Double]) extends CovFunc {

  private val Seq(logSf, logEll) = params

  private val opponentCovFunc = CovSEiso(logSf, logEll)
  def withParams(newParams: Seq[Double]): CovFunc = OpponentSeIsoCovFunc(params)
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {

    val (player1Vec, player2Vec) = if (player1.opponentName.equals(player2.opponentName)) (Array(0d), Array(0d)) else (Array(0d, 1), Array(1d, 0))
    val cov = opponentCovFunc.cov(player1Vec, player2Vec)
    cov

  }
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {
    val (player1Vec, player2Vec) = if (player1.opponentName.equals(player2.opponentName)) (Array(0d), Array(0d)) else (Array(0d, 1), Array(1d, 0))

    val covD = paramIndex match {
      case 0 => opponentCovFunc.df_dSf(player1Vec, player2Vec)
      case 1 => opponentCovFunc.df_dEll(player1Vec, player2Vec)
    }

    covD
  }
  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")

}