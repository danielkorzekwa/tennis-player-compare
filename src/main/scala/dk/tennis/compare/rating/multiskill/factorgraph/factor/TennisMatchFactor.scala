package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.TripleFactor
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel

case class TennisMatchFactor(p1SkillVarId: Int, p2SkillVarId: Int, outcomeVarId: Int,
  perfVariance: Double, p1Wins: IndexedSeq[Boolean]) extends TripleFactor {

  private val ZERO_PROBABILITY = 1.0E-20

  /**caching*/
  val skillCachingDelta = 0.000000001
  var prevP1Skill: Option[GaussianFactor] = None
  var prevP2Skill: Option[GaussianFactor] = None
  var prevP1Marginal: GaussianFactor = _
  var prevP2Marginal: GaussianFactor = _
  var prevOutcomeMarginal: SingleTableFactor = _

  def getVariableIds(): Seq[Int] = Vector(p1SkillVarId, p2SkillVarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `p1SkillVarId` => new GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `p2SkillVarId` => new GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `outcomeVarId` => outcomeMarginal(p1Wins.last)
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def productMarginal(varId: Int, factor1: Factor, factor2: Factor, factor3: Factor): SingleFactor = {

    productMarginalInternal(varId, factor1.asInstanceOf[GaussianFactor], factor2.asInstanceOf[GaussianFactor], factor3.asInstanceOf[SingleTableFactor])
  }

  private def productMarginalInternal(varId: Int, p1Skill: GaussianFactor, p2Skill: GaussianFactor, outcomeFactor: SingleTableFactor): SingleFactor = {

    if (!prevP1Skill.isDefined || !prevP1Skill.get.equals(p1Skill, skillCachingDelta) || !prevP2Skill.get.equals(p2Skill, skillCachingDelta)) {

      val (newP1Marginal, newP2Marginal) = p1Wins.foldLeft((p1Skill, p2Skill))((skills, win) => computeMarginals(skills._1, skills._2, win))

      val newOutcomeMarginal = outcomeMarginal(p1Wins.last)

      prevP1Marginal = newP1Marginal
      prevP2Marginal = newP2Marginal
      prevOutcomeMarginal = newOutcomeMarginal
      prevP1Skill = Some(p1Skill)
      prevP2Skill = Some(p2Skill)
    }

    val marginal = varId match {
      case `p1SkillVarId` => prevP1Marginal
      case `p2SkillVarId` => prevP2Marginal
      case `outcomeVarId` => prevOutcomeMarginal
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

    val p1_perf_to_diff = Gaussian(player1Skill.m, player1Skill.v + perfVariance)
    val p2_perf_to_diff = Gaussian(player2Skill.m, player2Skill.v + perfVariance)

    val diff_to_outcome = p1_perf_to_diff - p2_perf_to_diff
    val outcome_to_diff = diff_to_outcome.truncate(0, p1Wins) / diff_to_outcome

    val diff_to_perf1 = outcome_to_diff + p2_perf_to_diff
    val diff_to_perf2 = p1_perf_to_diff - outcome_to_diff

    val perf1_to_skill1 = Gaussian(diff_to_perf1.m, diff_to_perf1.v + perfVariance)
    val perf2_to_skill2 = Gaussian(diff_to_perf2.m, diff_to_perf2.v + perfVariance)

    val p1Marginal = Gaussian(player1Skill.m, player1Skill.v) * perf1_to_skill1
    val p2Marginal = Gaussian(player2Skill.m, player2Skill.v) * perf2_to_skill2

    val p1MarginalFactor = new GaussianFactor(player1Skill.varId, p1Marginal.m, p1Marginal.v)
    val p2MarginalFactor = new GaussianFactor(player2Skill.varId, p2Marginal.m, p2Marginal.v)
    (p1MarginalFactor, p2MarginalFactor)
  }

}