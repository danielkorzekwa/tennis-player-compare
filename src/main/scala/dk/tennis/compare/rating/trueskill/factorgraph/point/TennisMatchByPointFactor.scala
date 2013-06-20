package dk.tennis.compare.rating.trueskill.factorgraph.point

import scala.math._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.Factor
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.model.factor.TableFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.GaussianFactor

case class TennisMatchByPointFactor(p1SkillVarId: Int, p2SkillVarId: Int, outcomeVarId: Int,
  perfVariance: Double, player1Name: String, player2Name: String, pointResults: Seq[Result]) extends Factor {

  def getVariableIds(): Seq[Int] = Vector(p1SkillVarId, p2SkillVarId, outcomeVarId)

  def marginal(varId: Int): Factor = {
    val marginalFactor = varId match {

      case `p1SkillVarId` => GaussianFactor(varId, Double.NaN, Double.PositiveInfinity)
      case `p2SkillVarId` => GaussianFactor(varId, Double.NaN, Double.PositiveInfinity)
      case `outcomeVarId` => TableFactor(Vector(varId), Vector(2), Array(1d, 1d))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def productMarginal(varId: Int, factors: Seq[Factor]): Factor = {
    val marginal: Factor = factors match {
      case Seq(p1Skill: GaussianFactor, p2Skill: GaussianFactor, matchOutcome: TableFactor) if p1Skill.varId == p1SkillVarId && p2Skill.varId == p2SkillVarId && matchOutcome.variableIds.equals(List(outcomeVarId)) => {
        productMarginal(varId, p1Skill, p2Skill, matchOutcome)
      }
      case _ =>
        throw new IllegalArgumentException("TennisMatchFactor can be multiplied by exactly two gaussians and one table factor only")
    }
    marginal
  }

  private var prevP1Skill: Option[GaussianFactor] = None
  private var prevP2Skill: Option[GaussianFactor] = None
  private var prevProductMarginalP1Skill: Option[GaussianFactor] = None
  private var prevProductMarginalP2Skill: Option[GaussianFactor] = None

  private def productMarginal(varId: Int, p1Skill: GaussianFactor, p2Skill: GaussianFactor, outcomeFactor: TableFactor): Factor = {

    if (!prevP1Skill.isDefined || !p1Skill.equals(prevP1Skill.get, 0.00001)
      || !p2Skill.equals(prevP2Skill.get, 0.00001)) {

      val matchFactorGraph = TennisMatchByPointFactorGraph.create(player1Name, p1Skill, player2Name, p2Skill, perfVariance, pointResults)
      val ep = GenericEP(matchFactorGraph,threshold=0.001)

      def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
      val iterTotal = ep.calibrate(1000, progress)

      prevP1Skill = Some(p1Skill)
      prevP2Skill = Some(p2Skill)
      prevProductMarginalP1Skill = Some(ep.marginal(p1Skill.varId).asInstanceOf[GaussianFactor])
      prevProductMarginalP2Skill = Some(ep.marginal(p2Skill.varId).asInstanceOf[GaussianFactor])

    }

    val marginal = varId match {
      case `p1SkillVarId` => prevProductMarginalP1Skill.get
      case `p2SkillVarId` => prevProductMarginalP2Skill.get
      case `outcomeVarId` => TableFactor(Vector(varId), Vector(2), Array(1d, 1d))
      case _ => throw new IllegalArgumentException("Incorrect marginal variable id")
    }
    marginal
  }

  def withEvidence(varId: Int, varValue: AnyVal): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def getValue(assignment: (Int, AnyVal)*): Double = throw new UnsupportedOperationException("Not implemented yet")

  def *(factor: Factor): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def /(factor: Factor): Factor = throw new UnsupportedOperationException("Not implemented yet")

  def equals(that: Factor, threshold: Double): Boolean = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  //  private def createFactorGraph(skill1: TrueSkillRating, skill2: TrueSkillRating, playerPerfVar: Double): FactorGraph = {
  //    val skill1Factor = GaussianFactor(_skill1VarId, skill1.mean, skill1.variance)
  //    val skill2Factor = GaussianFactor(_skill2VarId, skill2.mean, skill2.variance)
  //    val perf1Factor = LinearGaussianFactor(_skill1VarId, _perf1VarId, 1, 0, playerPerfVar)
  //    val perf2Factor = LinearGaussianFactor(_skill2VarId, _perf2VarId, 1, 0, playerPerfVar)
  //    val perfDiffFactor = DiffGaussianFactor(_perf1VarId, _perf2VarId, _perfDiffVarId)
  //    val outcomeFactor = TruncGaussianFactor(_perfDiffVarId, _outcomeVarId, 0)
  //
  //    val factorGraph = GenericFactorGraph()
  //
  //    factorGraph.addFactor(skill1Factor)
  //    factorGraph.addFactor(skill2Factor)
  //    factorGraph.addFactor(perf1Factor)
  //    factorGraph.addFactor(perf2Factor)
  //    factorGraph.addFactor(perfDiffFactor)
  //    factorGraph.addFactor(outcomeFactor)
  //
  //    factorGraph
  //  }
}