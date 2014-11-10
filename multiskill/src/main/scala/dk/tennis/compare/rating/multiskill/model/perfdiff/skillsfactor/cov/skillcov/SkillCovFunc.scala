package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponentseiso.OpponentSeIsoCovFunc
import dk.bayes.infer.gp.cov.CovSEiso
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface.SurfaceCovFunc

case class SkillCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(scalingFactorLogSf, opponentLogSf, surfaceHardLogSf, surfaceClayLogSf, surfaceGrassLogSf) = params

  private val scalingFactorCov = CovSEiso(scalingFactorLogSf, log(1))
  private val opponentCovFunc = OpponentSeIsoCovFunc(Array(opponentLogSf))
  private val surfaceCovFunc = SurfaceCovFunc(Array(surfaceHardLogSf, surfaceClayLogSf, surfaceGrassLogSf))

  def withParams(newParams: Seq[Double]): CovFunc = SkillCovFunc(newParams)
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {

    scalingFactorCov.cov(0, 0) * opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2)
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val covD = paramIndex match {
      case 0 => scalingFactorCov.df_dSf(Matrix(0d), Matrix(0d)) * opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2)
      case 1 => scalingFactorCov.cov(0, 0) * opponentCovFunc.covarianceD(player1, player2, 0) * surfaceCovFunc.covariance(player1, player2)
      case 2 => scalingFactorCov.cov(0, 0) * opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 0)
      case 3 => scalingFactorCov.cov(0, 0) * opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 1)
      case 4 => scalingFactorCov.cov(0, 0) * opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 2)

    }

    covD
  }

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")
}