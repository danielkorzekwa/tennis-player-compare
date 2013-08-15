package dk.tennis.compare.rating.multiskill.factorgraph

import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import scala.collection._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.tennis.compare.rating.multiskill.domain.PointResult
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
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TournamentFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PriorSkillsFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TransitionSkillsFactor

case class TennisDbnFactorGraph2(multiSkillParams: MultiSkillParams) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skills variables*/
  private val skillsVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map()

  private val lastVarId = new AtomicInteger()

  def addTournament(tournament: TournamentResult) {

    val currSkillsVarIds: Seq[Int] = tournament.players.map(player => getSkillTransitionVarId(player))

    addTournamentToFactorGraph(currSkillsVarIds, tournament)

  }

  private def getSkillTransitionVarId(player: String): Int = {
    
    val playerSkillVars = skillsVarIds.getOrElseUpdate(player, {
      val priorSkillsFactor = PriorSkillsFactor(lastVarId.getAndIncrement())
      factorGraph.addFactor(priorSkillsFactor)
      ArrayBuffer(priorSkillsFactor.skillsVarId)
    })

    val transitionSkillsFactor = TransitionSkillsFactor(playerSkillVars.last, lastVarId.getAndIncrement())
    factorGraph.addFactor(transitionSkillsFactor)

    playerSkillVars += transitionSkillsFactor.skillsVarId

    transitionSkillsFactor.skillsVarId
  }

  private def addTournamentToFactorGraph(currSkillsVarIds: Seq[Int], tournament: TournamentResult) {

    val tournamentFactor = TournamentFactor()
    factorGraph.addFactor(tournamentFactor)
  }

  def getSkillsVarIds(): mutable.Map[String, ArrayBuffer[Int]] = skillsVarIds

  def getFactorGraph(): FactorGraph = factorGraph
}