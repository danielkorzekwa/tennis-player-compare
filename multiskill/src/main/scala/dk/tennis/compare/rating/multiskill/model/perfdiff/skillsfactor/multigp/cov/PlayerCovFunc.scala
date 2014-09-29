package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix

/**
 *  @param params (
 * logSf - shorm term covariance -log of signal standard deviation
 * logEll - shorm term covariance - log of length scale standard deviation
 * logSf - long term covariance -log of signal standard deviation
 * logEll - long term covariance - log of length scale standard deviation
 */
case class PlayerCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(logSfShort, logEllShort,
    logSfLong, logEllLong) = params

  private val DAY_MILLIS = 1000L * 3600 * 24

  def getParams(): Seq[Double] = params

  //basic covariances
  private val timeDiffCovShort = new CovSEiso(sf = logSfShort, logEllShort)
  private val timeDiffCovLong = new CovSEiso(sf = logSfLong, logEllLong)
  def onServeCov(player1: Player, player2: Player): Double = if (player1.onServe.equals(player2.onServe)) 1d else 0
  def theSamePlayerCov(player1: Player, player2: Player): Double = if (player1.playerName.equals(player2.playerName)) 1d else 0

  def covariance(player1: Player, player2: Player): Double = {
    val timeDiffCovShortVal = timeDiffCovShort.cov(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS))
    val timeDiffCovLongVal = timeDiffCovLong.cov(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS))

    onServeCov(player1, player2) * theSamePlayerCov(player1, player2) * (timeDiffCovShortVal + timeDiffCovLongVal)

  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {
    val covD = paramIndex match {
      case 0 => cov_df_d0(player1, player2)
      case 1 => cov_df_d1(player1, player2)
      case 2 => cov_df_d2(player1, player2)
      case 3 => cov_df_d3(player1, player2)
    }

    covD
  }

  private def cov_df_d0(player1: Player, player2: Player): Double = {
    onServeCov(player1, player2) * theSamePlayerCov(player1, player2) *
      (timeDiffCovShort.df_dSf(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS)) + 0)
  }

  private def cov_df_d1(player1: Player, player2: Player): Double = {
    onServeCov(player1, player2) * theSamePlayerCov(player1, player2) *
      (timeDiffCovShort.df_dEll(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS)) + 0)
  }

  private def cov_df_d2(player1: Player, player2: Player): Double = {
    onServeCov(player1, player2) * theSamePlayerCov(player1, player2) *
      (0 + timeDiffCovLong.df_dSf(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS)))
  }

  private def cov_df_d3(player1: Player, player2: Player): Double = {
    onServeCov(player1, player2) * theSamePlayerCov(player1, player2) *
      (0 + timeDiffCovLong.df_dEll(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS)) + 0)
  }

}