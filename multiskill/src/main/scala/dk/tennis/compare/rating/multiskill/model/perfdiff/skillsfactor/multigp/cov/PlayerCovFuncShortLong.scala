package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix

/**
 *  @param params (
 * logSf - day of match covariance -log of signal standard deviation
 * logEll - day of match covariance - log of length scale standard deviation
 * logSf - week of match covariance -log of signal standard deviation
 * logEll - week of match covariance - log of length scale standard deviation
 * logSf - month of match covariance -log of signal standard deviation
 * logEll - month of match covariance - log of length scale standard deviation
 * logSf - year of match covariance -log of signal standard deviation
 * logEll - year of match covariance - log of length scale standard deviation
 */
case class PlayerCovFuncShortLong(params: Seq[Double]) extends PlayerCovFunc {

  val Seq(logSfShort, logEllShort,
    logSfLong, logEllLong) = params

  //  val Seq(logSfShort, logEllShort) = params

  private val DAY_MILLIS = 1000L * 3600 * 24

  def covarianceMatrix(players: Array[Player]): Matrix = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-5

  private def covariance(player1: Player, player2: Player): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0
    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCovShort = new CovSEiso(sf = logSfShort, logEllShort).cov(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS))
    val timeDiffCovLong = new CovSEiso(sf = logSfLong, logEllLong).cov(Matrix(player1.timestamp.getTime.toDouble / DAY_MILLIS), Matrix(player2.timestamp.getTime.toDouble / DAY_MILLIS))
    onServeCov * theSamePlayerCov * (timeDiffCovShort + timeDiffCovLong)

  }

  def covarianceMatrixD(players: Array[Player]): Seq[Matrix] = {

    val covD_sf_short = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_sf(players(rowIndex), players(colIndex), logSfShort, logEllShort, DAY_MILLIS))
    val covD_ell_short = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), logSfShort, logEllShort, DAY_MILLIS))

    val covD_sf_long = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_sf(players(rowIndex), players(colIndex), logSfLong, logEllLong, DAY_MILLIS))
    val covD_ell_long = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), logSfLong, logEllLong, DAY_MILLIS))

    List(covD_sf_short, covD_ell_short, covD_sf_long, covD_ell_long)
  }

  private def covariance_df_sf(player1: Player, player2: Player, logSf: Double, logEll: Double, timePeriod: Long): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = logSf, logEll).df_dSf(Matrix(player1.timestamp.getTime.toDouble / timePeriod), Matrix(player2.timestamp.getTime.toDouble / timePeriod))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

  private def covariance_df_ell(player1: Player, player2: Player, logSf: Double, logEll: Double, timePeriod: Long): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = logSf, logEll).df_dEll(Matrix(player1.timestamp.getTime.toDouble / timePeriod), Matrix(player2.timestamp.getTime.toDouble / timePeriod))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

}