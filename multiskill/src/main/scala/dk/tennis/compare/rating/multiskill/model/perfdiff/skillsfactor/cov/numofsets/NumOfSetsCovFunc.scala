package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.numofsets

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import scala.math._
import dk.bayes.math.covfunc.CovSEiso
case class NumOfSetsCovFunc(params: Seq[Double]) extends CovFunc {

  private val Seq(logSf, logEll3Sets, logEll5Sets) = params

  private val cov3Sets = CovSEiso(log(1d), logEll3Sets)
  private val cov5Sets = CovSEiso(log(1d), logEll5Sets)

  def withParams(newParams: Seq[Double]): CovFunc = throw new UnsupportedOperationException("Not implemented yet")
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = throw new UnsupportedOperationException("Not implemented yet")

  def getParams(): Seq[Double] = params
  def covariance(player1: Player, player2: Player): Double = {

    val p1NumOfSets = Array(0, 0)
    p1NumOfSets(player1.numOfSets.id) = 1
    val p2NumOfSets = Array(0, 0)
    p2NumOfSets(player2.numOfSets.id) = 1

    val cov = exp(2 * logSf) * cov3Sets.cov(p1NumOfSets(0), p2NumOfSets(0)) * cov5Sets.cov(p1NumOfSets(1), p2NumOfSets(1))
    cov
  }
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    val p1NumOfSets = Array(0, 0)
    p1NumOfSets(player1.numOfSets.id) = 1

    val p2NumOfSets = Array(0, 0)
    p2NumOfSets(player2.numOfSets.id) = 1

    val covD = paramIndex match {
      case 0 => 2 * exp(2 * logSf) * cov3Sets.cov(p1NumOfSets(0), p2NumOfSets(0)) * cov5Sets.cov(p1NumOfSets(1), p2NumOfSets(1))
      case 1 => exp(2 * logSf) * cov3Sets.df_dEll(p1NumOfSets(0), p2NumOfSets(0)) * cov5Sets.cov(p1NumOfSets(1), p2NumOfSets(1))
      case 2 => exp(2 * logSf) * cov3Sets.cov(p1NumOfSets(0), p2NumOfSets(0)) * cov5Sets.df_dEll(p1NumOfSets(1), p2NumOfSets(1))

    }
    covD

  }

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")

  private implicit def toInt(x: Boolean): Double = if (x) 1 else 0
}