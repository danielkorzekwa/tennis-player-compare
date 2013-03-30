package dk.tennis.compare.rating.trueskill

import TrueSkill._
import scala.collection._
import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian
import scala.math._

case class GenericTrueSkill extends TrueSkill {

  private val skillsMap: mutable.Map[String, TrueSkillRating] = mutable.Map()

  private val defaultSkill = TrueSkillRating(0, 1)
  val skillTransitionFactor = LinearGaussian(1, 0, pow(25d / 300, 2))
  val performanceFactor = LinearGaussian(1, 0, pow(25d / 16, 2))

  def addResult(player1: String, player2: String, player1Win: Boolean) = {

    val player1Skill = skillsMap.getOrElse(player1, defaultSkill)
    val player2Skill = skillsMap.getOrElse(player2, defaultSkill)

    if (player1Win) {
      
      val (newPlayer1Skill, newPlayer2Skill) = computeMarginals(player1Skill, player2Skill, skillTransitionFactor, performanceFactor)
      skillsMap += player1 -> newPlayer1Skill
      skillsMap += player2 -> newPlayer2Skill
    } else {

      val (newPlayer2Skill, newPlayer1Skill) = computeMarginals(player2Skill, player1Skill, skillTransitionFactor, performanceFactor)
      skillsMap += player1 -> newPlayer1Skill
      skillsMap += player2 -> newPlayer2Skill
    }
  }

  def getRatings(): immutable.Map[String, TrueSkillRating] = skillsMap.toMap

  /**
   * Returns the probability of winning the game by player 1
   */
  def winnerProb(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating): Double = {

    val winProb = matchProbability(player1Skill, player2Skill, skillTransitionFactor, performanceFactor)
    winProb
  }

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner.
   *
   * Variables:
   * - S1_old Skill for player 1 at the time t-1,  N(m,v)
   * - S2_old Skill for player 2 at the time t-1,  N(m,v)
   * - S1 Skill for player 1 at the time t, N(m,v)
   * - S2 Skill for player 2 at the time t, N(m,v)
   * - P1 Performance for player 1, N(m,v)
   * - P2 Performance for player 2, N(m,v)
   *
   * - D|P1,P2 (Performance difference), N(m,v)
   * - O|D Outcome of the game from a point of view for player 1, {Win,Lose}
   *
   * Factors:
   * f0(S1_old),f1(S1_old,S1),f2(S1,P1),
   * f3(S2_old),f4(S2_old,S2),f5(S2,P2),
   * f6(P1,P2,D),f3(D,O)
   *
   * @return Posterior for [S1,S2]
   */
  private def computeMarginals(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating,
    skillTransitionFactor: LinearGaussian, performanceFactor: LinearGaussian): Tuple2[TrueSkillRating, TrueSkillRating] = {

    //forward messages for player 1
    val m_f0_to_f1 = Gaussian(player1Skill.skill, player1Skill.volatility)
    val m_f1_to_f2 = (skillTransitionFactor * m_f0_to_f1).marginalise(0)
    val m_f2_to_f6 = (performanceFactor * m_f1_to_f2).marginalise(0)

    //forward messages for player 2
    val m_f3_to_f4 = Gaussian(player2Skill.skill, player2Skill.volatility)
    val m_f4_to_f5 = (skillTransitionFactor * m_f3_to_f4).marginalise(0)
    val m_f5_to_f6 = (performanceFactor * m_f4_to_f5).marginalise(0)

    //forward-backward messages for performance difference
    val m_f6_to_f7 = m_f2_to_f6 - m_f5_to_f6
    val m_f7_to_f6 = m_f6_to_f7.truncateUpperTail(0) / m_f6_to_f7

    //backward messages for player 1
    val m_f6_to_f2 = m_f7_to_f6 + m_f5_to_f6
    val m_f2_to_f1 = (performanceFactor.toCanonical(xId = 1, yId = 2) * m_f6_to_f2.toCanonical(varId = 2)).marginalise(2).toGaussian()

    //backward messages for player 2
    val m_f6_to_f5 = m_f2_to_f6 - m_f7_to_f6
    val m_f5_to_f4 = (performanceFactor.toCanonical(xId = 1, yId = 2) * m_f6_to_f5.toCanonical(varId = 2)).marginalise(2).toGaussian()

    val s1Marginal = m_f2_to_f1 * m_f1_to_f2
    val s2Marginal = m_f5_to_f4 * m_f4_to_f5

    (TrueSkillRating(s1Marginal.m, s1Marginal.v), TrueSkillRating(s2Marginal.m, s2Marginal.v))
  }

  /**
   * Returns the probability of winning a tennis game by player1 against player2
   *
   */
  private def matchProbability(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating,
    skillTransitionFactor: LinearGaussian, performanceFactor: LinearGaussian): Double = {

    //forward messages for player 1
    val m_f0_to_f1 = Gaussian(player1Skill.skill, player1Skill.volatility)
    val m_f1_to_f2 = (skillTransitionFactor * m_f0_to_f1).marginalise(0)
    val m_f2_to_f6 = (performanceFactor * m_f1_to_f2).marginalise(0)

    //forward messages for player 2
    val m_f3_to_f4 = Gaussian(player2Skill.skill, player2Skill.volatility)
    val m_f4_to_f5 = (skillTransitionFactor * m_f3_to_f4).marginalise(0)
    val m_f5_to_f6 = (performanceFactor * m_f4_to_f5).marginalise(0)

    val m_f6_to_f7 = m_f2_to_f6 - m_f5_to_f6

    val outcomeMarginal = 1 - m_f6_to_f7.cdf(0)
    outcomeMarginal
  }

}