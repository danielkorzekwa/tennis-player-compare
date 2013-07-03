package dk.tennis.compare.rating.trueskill.factorgraph.tennismatch

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
import scala.collection.mutable.ListBuffer
import dk.bayes.model.factor.Factor

/**
 * Creates Dynamic Bayesian Network for tennis results.
 *
 * This is mutable class.
 *
 * @author Daniel Korzekwa
 *
 */
case class TennisDbnFactorGraph(skillTransVariance: Double, perfVariance: Double) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillVarIds: mutable.Map[String, ListBuffer[Int]] = mutable.Map[String, ListBuffer[Int]]()

  private val lastVarId = new AtomicInteger()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addResult(result: Result) {
    val player1PrevSkillVars = skillVarIds.getOrElseUpdate(result.player1, {
      val gaussianFactor = addSkillPriorFactor(defaultSkill)

      ListBuffer(gaussianFactor.varId)
    })

    val player2PrevSkillVars = skillVarIds.getOrElseUpdate(result.player2, {
      val gaussianFactor = addSkillPriorFactor(defaultSkill)

      ListBuffer(gaussianFactor.varId)
    })

    val skill1Factor = addSkillTransitionFactor(player1PrevSkillVars.last)
    val skill2Factor = addSkillTransitionFactor(player2PrevSkillVars.last)

    addTennisGameToFactorGraph(skill1Factor.varId, skill2Factor.varId, result.player1Win)

    player1PrevSkillVars += skill1Factor.varId
    player2PrevSkillVars += skill2Factor.varId

  }

  private def addSkillPriorFactor(playerSkill: TrueSkillRating): GaussianFactor = {
    val gaussianFactor = GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
    addFactorToFactorGraph(gaussianFactor)
    gaussianFactor
  }

  private def addSkillTransitionFactor(playerPrevSkillVarId: Int): LinearGaussianFactor = {
    val linearGaussianFactor = LinearGaussianFactor(playerPrevSkillVarId, lastVarId.getAndIncrement(), 1, 0, skillTransVariance)
    addFactorToFactorGraph(linearGaussianFactor)
    linearGaussianFactor
  }

  private def addTennisGameToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

    val perf1VarId = lastVarId.getAndIncrement()
    val perf2VarId = lastVarId.getAndIncrement()
    val perfDiffVarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    addFactorToFactorGraph(LinearGaussianFactor(player1VarId, perf1VarId, 1, 0, perfVariance))
    addFactorToFactorGraph(LinearGaussianFactor(player2VarId, perf2VarId, 1, 0, perfVariance))
    addFactorToFactorGraph(DiffGaussianFactor(perf1VarId, perf2VarId, perfDiffVarId))

    val outcomeFactor = TruncGaussianFactor(perfDiffVarId, outcomeVarId, 0)
    val outcomeFactorWithEvidence = if (player1Win) outcomeFactor.withEvidence(outcomeVarId, true)
    else outcomeFactor.withEvidence(outcomeVarId, false)
    addFactorToFactorGraph(outcomeFactorWithEvidence)
  }

  private def addFactorToFactorGraph(factor: Factor) {
    factorGraph.addFactor(factor)
  }

  /**@return Map[playerName, variable id]*/
  def getSkillVarIds(): immutable.Map[String, Seq[Int]] = skillVarIds.mapValues(varIds => varIds.toList).toMap

  def getFactorGraph(): FactorGraph = factorGraph
}