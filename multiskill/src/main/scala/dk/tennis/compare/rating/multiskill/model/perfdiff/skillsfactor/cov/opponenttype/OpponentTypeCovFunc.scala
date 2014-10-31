package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc

case class OpponentTypeCovFunc(params: Seq[Double], opponentTypeMap: Map[String, OpponentType]) extends CovFunc {

  val Seq(generalSkillLogSf, offensiveSkillLogSf, defensiveSkillLogSf) = params

  def withParams(params: Seq[Double]): CovFunc = throw new UnsupportedOperationException("Not implemented yet")
  
  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    val generalCov = exp(2 * generalSkillLogSf)

    val offensiveCov = exp(2 * offensiveSkillLogSf)
    val o1OffBit = opponentTypeMap(player1.opponentName).offensiveBit
    val o2OffBit = opponentTypeMap(player2.opponentName).offensiveBit

    val defensiveCov = exp(2 * defensiveSkillLogSf)
    val o1DefBit = opponentTypeMap(player1.opponentName).defensiveBit
    val o2DefBit = opponentTypeMap(player2.opponentName).defensiveBit

    val opponent1Type = opponentTypeMap(player1.opponentName).offensive
    val opponent2Type = opponentTypeMap(player2.opponentName).offensive

    val cov = generalCov + o1OffBit * offensiveCov * o2OffBit + o1DefBit * defensiveCov * o2DefBit

    cov
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {
    val covD = paramIndex match {
      case 0 => cov_df_d0(player1, player2)
      case 1 => cov_df_d1(player1, player2)
      case 2 => cov_df_d2(player1, player2)

    }
    covD
  }

  private def cov_df_d0(player1: Player, player2: Player): Double = {

    val covD = 2 * exp(2 * generalSkillLogSf)
    covD
  }

  private def cov_df_d1(player1: Player, player2: Player): Double = {
    val o1OffBit = opponentTypeMap(player1.opponentName).offensiveBit
    val o2OffBit = opponentTypeMap(player2.opponentName).offensiveBit
    o1OffBit * 2 * exp(2 * offensiveSkillLogSf) * o2OffBit
  }

  private def cov_df_d2(player1: Player, player2: Player): Double = {
    val o1DefBit = opponentTypeMap(player1.opponentName).defensiveBit
    val o2DefBit = opponentTypeMap(player2.opponentName).defensiveBit

    o1DefBit * 2 * exp(2 * defensiveSkillLogSf) * o2DefBit
  }

}