package dk.tennis.compare.rating.multiskill.factorgraph

import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import scala.collection._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.domain.PointResult
import scala.collection.mutable.ArrayBuffer
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.CachedTennisMatchFactor
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

case class TennisDbnFactorGraph(multiSkillParams: MultiSkillParams) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillOnServeVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()
  private val skillOnReturnVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

  private val lastVarId = new AtomicInteger()

  def addTennisMatch(matchResult: MatchResult) {

    val player1OnServeVarId = getSkillTransitionVarId(matchResult.player1, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
    val player1OnReturnVarId = getSkillTransitionVarId(matchResult.player1, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)

    val player2OnServeVarId = getSkillTransitionVarId(matchResult.player2, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
    val player2OnReturnVarId = getSkillTransitionVarId(matchResult.player2, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)

    addTennisMatchToFactorGraph(player1OnServeVarId, player1OnReturnVarId,
      player2OnServeVarId, player2OnReturnVarId, matchResult)

  }

  private def getSkillTransitionVarId(player: String, skillsVarIds: mutable.Map[String, ArrayBuffer[Int]], defaultSkill: PlayerSkill, skillTransVariance: Double): Int = {
    val playerSkillVars = skillsVarIds.getOrElseUpdate(player, {
      val gaussianFactor = new GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
      factorGraph.addFactor(gaussianFactor)
      ArrayBuffer(gaussianFactor.varId)
    })

    val linearGaussianFactor = LinearGaussianFactor(playerSkillVars.last, lastVarId.getAndIncrement(), 1, 0, skillTransVariance)
    factorGraph.addFactor(linearGaussianFactor)

    playerSkillVars += linearGaussianFactor.varId

    linearGaussianFactor.varId
  }

  private def addTennisMatchToFactorGraph(player1OnServeVarId: Int, player1OnReturnVarId: Int,
    player2OnServeVarId: Int, player2OnReturnVarId: Int, matchResult: MatchResult) {

    val player1VarId = lastVarId.getAndIncrement()
    val player2VarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    val player1Factor = PlayerFactor(player1OnServeVarId, player1OnReturnVarId, player1VarId)
    val player2Factor = PlayerFactor(player2OnServeVarId, player2OnReturnVarId, player2VarId)

    val matchFactor = new CachedTennisMatchFactor(player1VarId, player2VarId, outcomeVarId, multiSkillParams.perfVariance, matchResult)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)
  }

  def getSkillVarIdsOnServe(): mutable.Map[String, ArrayBuffer[Int]] = skillOnServeVarIds
  def getSkillVarIdsOnReturn(): mutable.Map[String, ArrayBuffer[Int]] = skillOnReturnVarIds

  def getFactorGraph(): FactorGraph = factorGraph
}