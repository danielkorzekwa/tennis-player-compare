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

case class SkillCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(
    opponentLogSf,
    surfaceHardLogSf, surfaceClayLogSf, surfaceGrassLogSf,
    logSfShort, logEllShort, logSfLong, logEllLong
    ) = params

  private val opponentCovFunc = OpponentSeIsoCovFunc(Array(log(1), opponentLogSf))
  private val surfaceCovFunc = SurfaceCovFunc(Array(log(1), surfaceHardLogSf, surfaceClayLogSf, surfaceGrassLogSf))
  private val overTimeCovFunc = SkillOverTimeCovFunc(Array(logSfShort, logEllShort, logSfLong, logEllLong))

  def withParams(newParams: Seq[Double]): CovFunc = SkillCovFunc(newParams)
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {
    opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covariance(player1, player2)
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val covD = paramIndex match {
      case 0 => opponentCovFunc.covarianceD(player1, player2, 1) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covariance(player1, player2)
      case 1 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 1) * overTimeCovFunc.covariance(player1, player2)
      case 2 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 2) * overTimeCovFunc.covariance(player1, player2)
      case 3 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covarianceD(player1, player2, 3) * overTimeCovFunc.covariance(player1, player2)
      case 4 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covarianceD(player1, player2, 0)
      case 5 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covarianceD(player1, player2, 1)
      case 6 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covarianceD(player1, player2, 2)
      case 7 => opponentCovFunc.covariance(player1, player2) * surfaceCovFunc.covariance(player1, player2) * overTimeCovFunc.covarianceD(player1, player2, 3)
    }

    covD
  }

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")
}