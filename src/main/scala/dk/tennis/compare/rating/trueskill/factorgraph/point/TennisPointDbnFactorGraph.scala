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
import dk.tennis.compare.rating.trueskill.factorgraph.tennismatch.factor.TennisMatchFactor

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

    addTennisMatchToFactorGraph(firstPointResult.player1, skill1Factor.varId, skill2Factor.varId, pointResults)
  //   addDeepTennisMatchToFactorGraph(firstPointResult.player1, firstPointResult.player2, skill1Factor.varId, skill2Factor.varId, pointResults)
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

  private def addDeepTennisMatchToFactorGraph(player1: String, player2: String, skill1VarId: Int, skill2VarId: Int, pointResults: Seq[Result]) {

    val outcomeVarId = lastVarId.getAndIncrement()
    val matchFactor = TennisMatchByPointFactor(skill1VarId, skill2VarId, outcomeVarId, perfVariance, player1, player2, pointResults)
    factorGraph.addFactor(matchFactor)
  }

  private def addTennisMatchToFactorGraph(player1: String, skill1VarId: Int, skill2VarId: Int, pointResults: Seq[Result]) {
    pointResults.foreach { r =>
      if (r.player1.equals(player1))
        addTennisPointToFactorGraph(skill1VarId, skill2VarId, r.player1Win)
      else addTennisPointToFactorGraph(skill2VarId, skill1VarId, r.player1Win)
    }
  }

  private def addTennisPointToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

     val outcomeVarId = lastVarId.getAndIncrement() 
    val tennisMatchFactor = TennisMatchFactor(player1VarId,player2VarId,outcomeVarId,perfVariance,Some(player1Win))
    factorGraph.addFactor(tennisMatchFactor)
    
  }

  def getFactorGraph(): FactorGraph = factorGraph
}