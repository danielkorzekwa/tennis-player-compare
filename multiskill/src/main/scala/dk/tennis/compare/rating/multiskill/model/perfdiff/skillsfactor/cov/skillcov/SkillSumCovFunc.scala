package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.covfunc.CovSEiso
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponentseiso.OpponentSeIsoCovFunc
import dk.bayes.math.covfunc.CovSEiso
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface.SurfaceCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.numofsets.NumOfSetsCovFunc

case class SkillSumCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(
    mainLogSf,
    opponentLogSf, opponentLogEll,
    surfaceLogSf, surfaceHardLogEll, surfaceClayLogEll, surfaceGrassLogEll,
    logSfShort, logEllShort, logSfLong, logEllLong
    ) = params

  private val opponentCovFunc = OpponentSeIsoCovFunc(Array(opponentLogSf, opponentLogSf))
  private val surfaceCovFunc = SurfaceCovFunc(Array(surfaceLogSf, surfaceHardLogEll, surfaceClayLogEll, surfaceGrassLogEll))
  private val overTimeCovFunc = SkillOverTimeCovFunc(Array(logSfShort, logEllShort, logSfLong, logEllLong))

  def withParams(newParams: Seq[Double]): CovFunc = SkillSumCovFunc(newParams)
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {
    (exp(2 * mainLogSf) + opponentCovFunc.covariance(player1, player2) + surfaceCovFunc.covariance(player1, player2)) * overTimeCovFunc.covariance(player1, player2)
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val covD = paramIndex match {

      case 0 => 2 * exp(2 * mainLogSf) * overTimeCovFunc.covariance(player1, player2)
      case 1 => opponentCovFunc.covarianceD(player1, player2, 0) * overTimeCovFunc.covariance(player1, player2)
      case 2 => opponentCovFunc.covarianceD(player1, player2, 1) * overTimeCovFunc.covariance(player1, player2)
      case 3 => surfaceCovFunc.covarianceD(player1, player2, 0) * overTimeCovFunc.covariance(player1, player2)
      case 4 => surfaceCovFunc.covarianceD(player1, player2, 1) * overTimeCovFunc.covariance(player1, player2)
      case 5 => surfaceCovFunc.covarianceD(player1, player2, 2) * overTimeCovFunc.covariance(player1, player2)
      case 6 => surfaceCovFunc.covarianceD(player1, player2, 3) * overTimeCovFunc.covariance(player1, player2)
      case 7 => (exp(2 * mainLogSf) + opponentCovFunc.covariance(player1, player2) + surfaceCovFunc.covariance(player1, player2)) * overTimeCovFunc.covarianceD(player1, player2, 0)
      case 8 => (exp(2 * mainLogSf) + opponentCovFunc.covariance(player1, player2) + surfaceCovFunc.covariance(player1, player2)) * overTimeCovFunc.covarianceD(player1, player2, 1)
      case 9 => (exp(2 * mainLogSf) + opponentCovFunc.covariance(player1, player2) + surfaceCovFunc.covariance(player1, player2)) * overTimeCovFunc.covarianceD(player1, player2, 2)
      case 10 => (exp(2 * mainLogSf) + opponentCovFunc.covariance(player1, player2) + surfaceCovFunc.covariance(player1, player2)) * overTimeCovFunc.covarianceD(player1, player2, 3)
    }

    covD
  }

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")
}