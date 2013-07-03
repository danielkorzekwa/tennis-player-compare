package dk.tennis.compare.rating.trueskill.factorgraph.tennismatch.factor

import dk.bayes.model.factor.Factor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.gaussian.MultivariateGaussian.toGaussian
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.SingleFactor

case class TennisMatchFactor(p1SkillVarId: Int, p2SkillVarId: Int, outcomeVarId: Int,
  perfVariance: Double, p1Wins: Option[Boolean] = None) extends Factor {

  private val ZERO_PROBABILITY = 1.0E-20

  private val _skill1VarId = 1
  private val _skill2VarId = 2
  private val _perf1VarId = 3
  private val _perf2VarId = 4
  private val _perfDiffVarId = 5
  private val _outcomeVarId = 6

  /**caching*/
  val skillCachingDelta = 0.000000001
  var prevP1Skill: Option[GaussianFactor] = None
  var prevP2Skill: Option[GaussianFactor] = None
  var prevP1Marginal: Option[GaussianFactor] = None
  var prevP2Marginal: Option[GaussianFactor] = None
  var prevOutcomeMarginal: Option[SingleTableFactor] = None

  def getVariableIds(): Seq[Int] = Vector(p1SkillVarId, p2SkillVarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `p1SkillVarId` => GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `p2SkillVarId` => GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `outcomeVarId` => p1Wins match {
        case None => SingleTableFactor(varId, 2, Array(1d, 1d))
        case Some(p1Wins) => outcomeMarginal(p1Wins)
      }
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def productMarginal(varId: Int, factors: Seq[Factor]): SingleFactor = {
    val marginal: SingleFactor = factors match {
      case Seq(p1Skill: GaussianFactor, p2Skill: GaussianFactor, matchOutcome: SingleTableFactor) 
      if (p1Skill.varId == p1SkillVarId && p2Skill.varId == p2SkillVarId && matchOutcome.varId==outcomeVarId) => {
        productMarginalInternal(varId, p1Skill, p2Skill, matchOutcome)
      }
      case _ =>
        throw new IllegalArgumentException("TennisMatchFactor can be multiplied by exactly two gaussians and one table factor only")
    }
    marginal
  }

  private def productMarginalInternal(varId: Int, p1Skill: GaussianFactor, p2Skill: GaussianFactor, outcomeFactor: SingleTableFactor): SingleFactor = {

    if (!prevP1Skill.isDefined || !prevP1Skill.get.equals(p1Skill, skillCachingDelta) || !prevP2Skill.get.equals(p2Skill, skillCachingDelta)) {

      val (newP1Marginal, newP2Marginal) = p1Wins match {
        case None => (p1Skill, p2Skill)
        case Some(p1Wins) => computeMarginals(p1Skill, p2Skill, p1Wins)
      }

      val newOutcomeMarginal = p1Wins match {
        case None => matchProbability(p1Skill, p2Skill)
        case Some(p1Wins) => outcomeMarginal(p1Wins)
      }

      prevP1Marginal = Some(newP1Marginal)
      prevP2Marginal = Some(newP2Marginal)
      prevOutcomeMarginal = Some(newOutcomeMarginal)
      prevP1Skill = Some(p1Skill)
      prevP2Skill = Some(p2Skill)
    }

    val marginal = varId match {
      case `p1SkillVarId` => prevP1Marginal.get
      case `p2SkillVarId` => prevP2Marginal.get
      case `outcomeVarId` => prevOutcomeMarginal.get
      case _ => throw new IllegalArgumentException("Incorrect marginal variable id")
    }
    marginal
  }

  private def outcomeMarginal(p1Wins: Boolean): SingleTableFactor = {
    p1Wins match {
      case true => SingleTableFactor(outcomeVarId, 2, Array(1 - ZERO_PROBABILITY, ZERO_PROBABILITY))
      case false => SingleTableFactor(outcomeVarId, 2, Array(ZERO_PROBABILITY, 1 - ZERO_PROBABILITY))
    }
  }

  /**
   * Returns posterior marginals for player1 and player2 given player1 is a winner.
   *
   * @return Posterior for [P1Marginal,P2Marginal]
   */
  private def computeMarginals(player1Skill: GaussianFactor, player2Skill: GaussianFactor, p1Wins: Boolean): Tuple2[GaussianFactor, GaussianFactor] = {

    val p1_perf_to_diff = (LinearGaussian(1, 0, perfVariance) * Gaussian(player1Skill.m, player1Skill.v)).marginalise(0).toGaussian()
    val p2_perf_to_diff = (LinearGaussian(1, 0, perfVariance) * Gaussian(player2Skill.m, player2Skill.v)).marginalise(0).toGaussian()

    val diff_to_outcome = p1_perf_to_diff - p2_perf_to_diff
    val outcome_to_diff = diff_to_outcome.truncate(0, p1Wins) / diff_to_outcome

    val diff_to_perf1 = outcome_to_diff + p2_perf_to_diff
    val diff_to_perf2 = p1_perf_to_diff - outcome_to_diff

    val perf1_to_skill1 = Gaussian(diff_to_perf1.m, diff_to_perf1.v + perfVariance)
    val perf2_to_skill2 = Gaussian(diff_to_perf2.m, diff_to_perf2.v + perfVariance)

    val p1Marginal = Gaussian(player1Skill.m, player1Skill.v) * perf1_to_skill1
    val p2Marginal = Gaussian(player2Skill.m, player2Skill.v) * perf2_to_skill2

    val p1MarginalFactor = GaussianFactor(player1Skill.varId, p1Marginal.m, p1Marginal.v)
    val p2MarginalFactor = GaussianFactor(player2Skill.varId, p2Marginal.m, p2Marginal.v)
    (p1MarginalFactor, p2MarginalFactor)
  }

  /**
   * Returns the probability of winning a tennis game by player1 against player2
   *
   */
  private def matchProbability(player1Skill: GaussianFactor, player2Skill: GaussianFactor): SingleTableFactor = {

    val p1_perf_to_diff = (LinearGaussian(1, 0, perfVariance) * Gaussian(player1Skill.m, player1Skill.v)).marginalise(0).toGaussian()
    val p2_perf_to_diff = (LinearGaussian(1, 0, perfVariance) * Gaussian(player2Skill.m, player2Skill.v)).marginalise(0).toGaussian()

    val diff_to_outcome = p1_perf_to_diff - p2_perf_to_diff
    val value0Prob = 1 - diff_to_outcome.cdf(0)

    val value1Prob = 1 - value0Prob
    val valueProbs = Array(value0Prob, value1Prob)

    val outcomeMarginal = SingleTableFactor(outcomeVarId, 2, valueProbs)
    outcomeMarginal
  }

  def withEvidence(varId: Int, varValue: AnyVal): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def getValue(assignment: (Int, AnyVal)*): Double = throw new UnsupportedOperationException("Not implemented yet")

  def *(factor: Factor): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def /(factor: Factor): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def equals(that: Factor, threshold: Double): Boolean = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

}