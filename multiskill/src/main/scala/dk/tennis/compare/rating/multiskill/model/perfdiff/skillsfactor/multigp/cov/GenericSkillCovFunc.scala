package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

case class GenericSkillCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(logSfShort, logEllShort, logSfLong, logEllLong,
    theSameOpponentLogSf, everyOpponentLogSf) = params

  //basic covariances
  private val playerCov = PlayerCovFunc(List(logSfShort, logEllShort, logSfLong, logEllLong))
  private val opponentCov = OpponentCovFunc(List(theSameOpponentLogSf, everyOpponentLogSf))

  def covariance(player1: Player, player2: Player): Double = {
    playerCov.covariance(player1, player2) * opponentCov.covariance(player1, player2)
  }
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val covD = paramIndex match {
      case 0 => playerCov.covarianceD(player1, player2, 0) * opponentCov.covariance(player1, player2)
      case 1 => playerCov.covarianceD(player1, player2, 1) * opponentCov.covariance(player1, player2)
      case 2 => playerCov.covarianceD(player1, player2, 2) * opponentCov.covariance(player1, player2)
      case 3 => playerCov.covarianceD(player1, player2, 3) * opponentCov.covariance(player1, player2)
      case 4 => playerCov.covariance(player1, player2) * opponentCov.covarianceD(player1, player2, 0)
      case 5 => playerCov.covariance(player1, player2) * opponentCov.covarianceD(player1, player2, 1)
    }

    covD
  }

  def getParams(): Seq[Double] = params
}