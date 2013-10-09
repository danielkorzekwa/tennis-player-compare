package dk.tennis.compare.rating.multiskill.factorgraph

import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import scala.collection._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import scala.collection.mutable.ArrayBuffer
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.CachedTennisMatchFactor
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

case class TennisDbnFactorGraph(multiSkillParams: MultiSkillParams) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillOnServeVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()
  private val skillOnReturnVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

  private val tennisMatchFactors: ArrayBuffer[TennisMatchFactor] = ArrayBuffer[TennisMatchFactor]()

  private val lastVarId = new AtomicInteger()

  def addTournament(tournament: TournamentResult) {

    tournament.players.foreach { p =>
      getSkillTransitionVarId(p, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
      getSkillTransitionVarId(p, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)
    }

    tournament.matchResults.foreach { matchResult =>
      val player1OnServeVarId = skillOnServeVarIds(matchResult.player1).last
      val player1OnReturnVarId = skillOnReturnVarIds(matchResult.player1).last

      val player2OnServeVarId = skillOnServeVarIds(matchResult.player2).last
      val player2OnReturnVarId = skillOnReturnVarIds(matchResult.player2).last

      addTennisMatchToFactorGraph(player1OnServeVarId, player1OnReturnVarId,
        player2OnServeVarId, player2OnReturnVarId, matchResult)
    }

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

    val matchFactor = new CachedTennisMatchFactor(player1Factor, player2Factor, outcomeVarId, multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn, matchResult)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)

    tennisMatchFactors += matchFactor
  }

  def getSkillVarIdsOnServe(): mutable.Map[String, ArrayBuffer[Int]] = skillOnServeVarIds
  def getSkillVarIdsOnReturn(): mutable.Map[String, ArrayBuffer[Int]] = skillOnReturnVarIds

  def getFactorGraph(): FactorGraph = factorGraph

  def getTennisMatchFactors(): ArrayBuffer[TennisMatchFactor] = tennisMatchFactors
}