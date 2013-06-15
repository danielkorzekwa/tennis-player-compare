package dk.tennis.compare.rating.trueskill.factorgraph.deepdbn

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
import scala.collection.mutable.ListBuffer

case class TennisDeepDbnFactorGraph (skillTransVariance: Double, perfVariance: Double) {

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
    factorGraph.addFactor(gaussianFactor)
    gaussianFactor
  }

  private def addSkillTransitionFactor(playerPrevSkillVarId: Int): LinearGaussianFactor = {
    val linearGaussianFactor = LinearGaussianFactor(playerPrevSkillVarId, lastVarId.getAndIncrement(), 1, 0, skillTransVariance)
    factorGraph.addFactor(linearGaussianFactor)
    linearGaussianFactor
  }

  private def addTennisGameToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

    val outcomeVarId = lastVarId.getAndIncrement() 
    val tennisMatchFactor = TennisMatchFactor(player1VarId,player2VarId,outcomeVarId,perfVariance,Some(player1Win))
    factorGraph.addFactor(tennisMatchFactor)
  }

  /**@return Map[playerName, variable id]*/
  def getSkillVarIds(): immutable.Map[String, Seq[Int]] = skillVarIds.mapValues(varIds => varIds.toList).toMap

  def getFactorGraph(): FactorGraph = factorGraph
}