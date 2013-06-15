package dk.tennis.compare.rating.trueskill.factorgraph.deepdbn

import org.junit._
import Assert._
import scala.math._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.GenericEP

class TennisMatchFactorTest {

  val p1SkillVarId = 10
  val p2SkillVarId = 20
  val outcomeVarId = 30

  val perfVariance = pow(25d / 6, 2)

  @Test def variable_marginal_no_result_set {

    val p1SkillFactor = GaussianFactor(p1SkillVarId, 27.1743, 37.5013)
    val p2SkillFactor = GaussianFactor(p2SkillVarId, 33.8460, 20.8610)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance)

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)

    assertEquals(1, ep.calibrate(100, progress))

    val outcomeMarginal = ep.marginal(outcomeVarId)
    assertEquals(0.24463, outcomeMarginal.getValue((outcomeVarId, 0)), 0.00001)
    assertEquals(0.75537, outcomeMarginal.getValue((outcomeVarId, 1)), 0.00001)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(27.1742, skill1Marginal.m, 0.0001)
    assertEquals(37.5013, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(33.846, skill2Marginal.m, 0.0001)
    assertEquals(20.861, skill2Marginal.v, 0.0001)

  }
  @Test def variable_marginal_player1_wins {
    val p1SkillFactor = GaussianFactor(p1SkillVarId, 4, 81)
    val p2SkillFactor = GaussianFactor(p2SkillVarId, 41, 25)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance, Some(true))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)

    assertEquals(2, ep.calibrate(100, progress))

    val outcomeMarginal = ep.marginal(outcomeVarId)
    assertEquals(1, outcomeMarginal.getValue((outcomeVarId, 0)), 0.00001)
    assertEquals(0, outcomeMarginal.getValue((outcomeVarId, 1)), 0.00001)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(27.1744, skill1Marginal.m, 0.0001)
    assertEquals(37.4973, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(33.8473, skill2Marginal.m, 0.0001)
    assertEquals(20.8559, skill2Marginal.v, 0.0001)
  }

  private def progress(currIter: Int) = println("EP iteration: " + currIter)
}