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
      val gaussianFactor = addSkillPriorFactor(defaultSkill)
      gaussianFactor.varId
    })

    val player2PrevSkillVarId = latestSkillVarIds.getOrElseUpdate(result.player2, {
      val gaussianFactor = addSkillPriorFactor(defaultSkill)
      gaussianFactor.varId
    })

    //  val skill1Factor = addSkillTransitionFactor(player1PrevSkillVarId)
    //  val skill2Factor = addSkillTransitionFactor(player2PrevSkillVarId)

    //  addTennisGameToFactorGraph(skill1Factor.varId, skill2Factor.varId, result.player1Win)
    addTennisGameToFactorGraph(player1PrevSkillVarId, player2PrevSkillVarId, result.player1Win)
  }

  private def addSkillPriorFactor(playerSkill: TrueSkillRating): GaussianFactor = {
    val gaussianFactor = GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
    factorGraph.addFactor(gaussianFactor)
    gaussianFactor
  }

  private def addSkillTransitionFactor(playerPrevSkillVarId: Int): LinearGaussianFactor = {
    val linearGaussianFactor = LinearGaussianFactor(playerPrevSkillVarId, lastVarId.getAndIncrement(), 1, 0, skillTransVariance)
    factorGraph.addFactor(linearGaussianFactor)
    linearGaussianFactor
  }

  private def addTennisGameToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

    val perf1VarId = lastVarId.getAndIncrement()
    val perf2VarId = lastVarId.getAndIncrement()
    val perfDiffVarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    factorGraph.addFactor(LinearGaussianFactor(player1VarId, perf1VarId, 1, 0, perfVariance))
    factorGraph.addFactor(LinearGaussianFactor(player2VarId, perf2VarId, 1, 0, perfVariance))
    factorGraph.addFactor(DiffGaussianFactor(perf1VarId, perf2VarId, perfDiffVarId))

    val outcomeFactor = TruncGaussianFactor(perfDiffVarId, outcomeVarId, 0)
    val outcomeFactorWithEvidence = if (player1Win) outcomeFactor.withEvidence(outcomeVarId, 0)
    else outcomeFactor.withEvidence(outcomeVarId, 1)
    factorGraph.addFactor(outcomeFactorWithEvidence)
  }

  /**@return Map[playerName, variable id]*/
  def getLatestSkillVarIds(): immutable.Map[String, Int] = latestSkillVarIds.toMap

  def getFactorGraph(): FactorGraph = factorGraph
}