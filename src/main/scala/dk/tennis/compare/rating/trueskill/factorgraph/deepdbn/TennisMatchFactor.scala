package dk.tennis.compare.rating.trueskill.factorgraph.deepdbn

import dk.bayes.model.factor.Factor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.TableFactor
import dk.bayes.model.factor.TableFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.TableFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.factorgraph.SingleGameFactorGraph
import dk.bayes.infer.LoopyBP
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph

case class TennisMatchFactor(p1SkillVarId: Int, p2SkillVarId: Int, outcomeVarId: Int,
  perfVariance: Double, p1Wins: Option[Boolean] = None) extends Factor {

  private val _skill1VarId = 1
  private val _skill2VarId = 2
  private val _perf1VarId = 3
  private val _perf2VarId = 4
  private val _perfDiffVarId = 5
  private val _outcomeVarId = 6

  def getVariableIds(): Seq[Int] = Vector(p1SkillVarId, p2SkillVarId, outcomeVarId)

  def marginal(varId: Int): Factor = {
    val marginalFactor = varId match {

      case `p1SkillVarId` => GaussianFactor(varId, Double.NaN, Double.PositiveInfinity)
      case `p2SkillVarId` => GaussianFactor(varId, Double.NaN, Double.PositiveInfinity)
      case `outcomeVarId` => p1Wins match {
        case None => TableFactor(Vector(varId), Vector(2), Array(1d, 1d))
        case Some(true) => TableFactor(Vector(varId), Vector(2), Array(1d, 0d))
        case Some(false) => TableFactor(Vector(varId), Vector(2), Array(0d, 1d))
      }
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

  private def productMarginal(varId: Int, p1Skill: GaussianFactor, p2Skill: GaussianFactor, outcomeFactor: TableFactor): Factor = {

    val p1Rating = TrueSkillRating(p1Skill.m, p1Skill.v)
    val p2Rating = TrueSkillRating(p2Skill.m, p2Skill.v)
    val matchFactorGraph = createFactorGraph(p1Rating, p2Rating, perfVariance)

    val ep = GenericEP(matchFactorGraph)

    if (p1Wins.isDefined) {
      if (p1Wins.get) ep.setEvidence(_outcomeVarId, 0)
      else ep.setEvidence(_outcomeVarId, 1)
    }

    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
    val iterTotal = ep.calibrate(100, progress)

    val skill1Marginal = ep.marginal(_skill1VarId).asInstanceOf[GaussianFactor]
    val skill2Marginal = ep.marginal(_skill2VarId).asInstanceOf[GaussianFactor]
    val outcomeMarginal = ep.marginal(_outcomeVarId).asInstanceOf[TableFactor]
    val marginal = varId match {
      case `p1SkillVarId` => GaussianFactor(p1SkillVarId, skill1Marginal.m, skill1Marginal.v)
      case `p2SkillVarId` => GaussianFactor(p2SkillVarId, skill2Marginal.m, skill2Marginal.v)
      case `outcomeVarId` => TableFactor(List(outcomeVarId), outcomeMarginal.variableDims, outcomeMarginal.valueProbs)
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

  private def createFactorGraph(skill1: TrueSkillRating, skill2: TrueSkillRating, playerPerfVar: Double): FactorGraph = {
    val skill1Factor = GaussianFactor(_skill1VarId, skill1.mean, skill1.variance)
    val skill2Factor = GaussianFactor(_skill2VarId, skill2.mean, skill2.variance)
    val perf1Factor = LinearGaussianFactor(_skill1VarId, _perf1VarId, 1, 0, playerPerfVar)
    val perf2Factor = LinearGaussianFactor(_skill2VarId, _perf2VarId, 1, 0, playerPerfVar)
    val perfDiffFactor = DiffGaussianFactor(_perf1VarId, _perf2VarId, _perfDiffVarId)
    val outcomeFactor = TruncGaussianFactor(_perfDiffVarId, _outcomeVarId, 0)

    val factorGraph = GenericFactorGraph()

    factorGraph.addFactor(skill1Factor)
    factorGraph.addFactor(skill2Factor)
    factorGraph.addFactor(perf1Factor)
    factorGraph.addFactor(perf2Factor)
    factorGraph.addFactor(perfDiffFactor)
    factorGraph.addFactor(outcomeFactor)

    factorGraph
  }
}