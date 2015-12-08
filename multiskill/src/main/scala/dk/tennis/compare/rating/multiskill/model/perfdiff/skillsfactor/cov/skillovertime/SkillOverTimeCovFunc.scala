package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime

import dk.bayes.math.covfunc.CovSEiso
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import scala.io.Source
import org.apache.commons.io.FileUtils
import java.io.File

case class SkillOverTimeCovFunc(params: Seq[Double]) extends CovFunc {

  val Seq(logSfShort, logEllShort,
    logSfLong, logEllLong) = params

  private val DAY_MILLIS = 1000L * 3600 * 24

  def withParams(params: Seq[Double]): SkillOverTimeCovFunc = {
    new SkillOverTimeCovFunc(params)
  }

  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  def getParams(): Seq[Double] = params

  //basic covariances
  private val timeDiffCovShort = new CovSEiso(sf = logSfShort, logEllShort)
  private val timeDiffCovLong = new CovSEiso(sf = logSfLong, logEllLong)

  def covariance(player1: Player, player2: Player): Double = {
    val timeDiffCovShortVal = timeDiffCovShort.cov(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS)
    val timeDiffCovLongVal = timeDiffCovLong.cov(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS)

    val cov = timeDiffCovShortVal + timeDiffCovLongVal
    cov

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

    timeDiffCovShort.df_dSf(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS) + 0
  }

  private def cov_df_d1(player1: Player, player2: Player): Double = {

    timeDiffCovShort.df_dEll(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS) + 0
  }

  private def cov_df_d2(player1: Player, player2: Player): Double = {

    0 + timeDiffCovLong.df_dSf(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS)
  }

  private def cov_df_d3(player1: Player, player2: Player): Double = {

    0 + timeDiffCovLong.df_dEll(player1.timestamp.getTime.toDouble / DAY_MILLIS, player2.timestamp.getTime.toDouble / DAY_MILLIS) + 0
  }

  def save(file: String) = {
    FileUtils.write(new File(file), params.mkString(","))
  }
}

object SkillOverTimeCovFunc {
  def fromFile(file: String): SkillOverTimeCovFunc = {
    val params = Source.fromFile(file).getLines.toList.head.split(",").map(v => v.toDouble)
    SkillOverTimeCovFunc(params)
  }
}