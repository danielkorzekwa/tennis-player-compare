package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.bayes.math.covfunc.CovSEiso
import dk.bayes.math.covfunc.CovSEiso
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface._
import breeze.linalg.DenseMatrix

case class SurfaceCovFunc(params: Seq[Double]) extends CovFunc {

  private val Seq(logSf, logEllHard, logEllClay, logEllGrass) = params

  private val covHard = CovSEiso(log(1d), logEllHard)
  private val covClay = CovSEiso(log(1d), logEllClay)
  private val covGrass = CovSEiso(log(1d), logEllGrass)

  def withParams(newParams: Seq[Double]): CovFunc = throw new UnsupportedOperationException("Not implemented yet")
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {

    val p1Surf = Array(0, 0, 0)
    p1Surf(player1.surface.id) = 1

    val p2Surf = Array(0, 0, 0)
    p2Surf(player2.surface.id) = 1

    val cov = exp(2 * logSf) * covHard.cov(p1Surf(0), p2Surf(0)) * covClay.cov(p1Surf(1), p2Surf(1)) * covGrass.cov(p1Surf(2), p2Surf(2))
    cov
  }
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val p1Surf = Array(0.0, 0, 0)
    p1Surf(player1.surface.id) = 1

    val p2Surf = Array(0.0, 0, 0)
    p2Surf(player2.surface.id) = 1

    val covD = paramIndex match {
      case 0 => 2 * exp(2 * logSf) * covHard.cov(p1Surf(0), p2Surf(0)) * covClay.cov(p1Surf(1),p2Surf(1)) * covGrass.cov(p1Surf(2), p2Surf(2))
      case 1 => exp(2 * logSf) * covHard.df_dEll(p1Surf(0), p2Surf(0)) * covClay.cov(p1Surf(1), p2Surf(1)) * covGrass.cov(p1Surf(2), p2Surf(2))
      case 2 => exp(2 * logSf) * covHard.cov(p1Surf(0), p2Surf(0)) * covClay.df_dEll(p1Surf(1), p2Surf(1)) * covGrass.cov(p1Surf(2), p2Surf(2))
      case 3 => exp(2 * logSf) * covHard.cov(p1Surf(0), p2Surf(0)) * covClay.cov(p1Surf(1), p2Surf(1)) * covGrass.df_dEll(p1Surf(2), p2Surf(2))

    }
    covD

  }

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")

  private implicit def toInt(x: Boolean): Double = if (x) 1 else 0
}