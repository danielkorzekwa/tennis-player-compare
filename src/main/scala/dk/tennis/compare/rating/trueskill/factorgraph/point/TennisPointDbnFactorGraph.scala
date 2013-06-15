package dk.tennis.compare.rating.trueskill.factorgraph.point

import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import scala.collection._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor

case class TennisPointDbnFactorGraph(skillTransVariance: Double, perfVariance: Double) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillVarIds: mutable.Map[String, ListBuffer[Int]] = mutable.Map[String, ListBuffer[Int]]()

  private val lastVarId = new AtomicInteger()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addPointResults(pointResults: Seq[Result]) {

    val firstPointResult = pointResults.head

    val player1PrevSkillVars = skillVarIds.getOrElseUpdate(firstPointResult.player1, {
      val gaussianFactor = addSkillPriorFactor(defaultSkill)
      ListBuffer(gaussianFactor.varId)
    })

    val player2PrevSkillVars = skillVarIds.getOrElseUpdate(firstPointResult.player2, {
      val gaussianFactor = addSkillPriorFactor(defaultSkill)
      ListBuffer(gaussianFactor.varId)
    })

    val skill1Factor = addSkillTransitionFactor(player1PrevSkillVars.last)
    val skill2Factor = addSkillTransitionFactor(player2PrevSkillVars.last)

    player1PrevSkillVars += skill1Factor.varId
    player2PrevSkillVars += skill2Factor.varId

    pointResults.foreach { r =>

      if (r.player1.equals(firstPointResult.player1))
        addTennisGameToFactorGraph(skill1Factor.varId, skill2Factor.varId, r.player1Win)
      else addTennisGameToFactorGraph(skill2Factor.varId, skill1Factor.varId, r.player1Win)
    }

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

  def getFactorGraph(): FactorGraph = factorGraph
}