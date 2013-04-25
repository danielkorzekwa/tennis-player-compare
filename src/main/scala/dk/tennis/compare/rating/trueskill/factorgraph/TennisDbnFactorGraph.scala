package dk.tennis.compare.rating.trueskill.factorgraph

import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.model.factorgraph.GenericFactorGraph
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.bayes.model.factor.LinearGaussianFactor
import scala.math._
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.infer.ep.GenericEP

case class TennisDbnFactorGraph(skillTransVariance: Double, perfVariance: Double) {

  private val factorGraph = GenericFactorGraph()
  private val latestSkillVarIds: mutable.Map[String, Int] = mutable.Map[String, Int]()

  private val lastVarId = new AtomicInteger()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addResult(result: Result) {
    val player1PrevSkillVarId = latestSkillVarIds.getOrElseUpdate(result.player1, {
      val gaussianFactor = addSkillFactor(defaultSkill)
      gaussianFactor.varId
    })

    val player2PrevSkillVarId = latestSkillVarIds.getOrElseUpdate(result.player2, {
      val gaussianFactor = addSkillFactor(defaultSkill)
      gaussianFactor.varId
    })

    addTennisGameToFactorGraph(player1PrevSkillVarId, player2PrevSkillVarId, result.player1Win)
  }

  private def addSkillFactor(playerSkill: TrueSkillRating): GaussianFactor = {
    val gaussianFactor = GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
    factorGraph.addFactor(gaussianFactor)

    gaussianFactor
  }

  private def addTennisGameToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

    val perf1VarId = lastVarId.getAndIncrement()
    val perf2VarId = lastVarId.getAndIncrement()
    val perfDiffVarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    factorGraph.addFactor(LinearGaussianFactor(player1VarId, perf1VarId, 1, 0, perfVariance))
    factorGraph.addFactor(LinearGaussianFactor(player2VarId, perf2VarId, 1, 0, perfVariance))
    factorGraph.addFactor(DiffGaussianFactor(perf1VarId, perf2VarId, perfDiffVarId))
    factorGraph.addFactor(TruncGaussianFactor(perfDiffVarId, outcomeVarId, 0))

    if (player1Win) GenericEP(factorGraph).setEvidence(outcomeVarId, 0)
    else GenericEP(factorGraph).setEvidence(outcomeVarId, 1)
  }

  /**@return Map[playerName, variable id]*/
  def getLatestSkillVarIds(): immutable.Map[String, Int] = throw new UnsupportedOperationException("Not implemented yet")

  def getFactorGraph(): FactorGraph = {
    GenericFactorGraph()
  }
}