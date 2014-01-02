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
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.bayes.infer.ep.GenericEP
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import java.util.Date

case class TennisDbnFactorGraph(multiSkillParams: MultiSkillParams) {

  private val factorGraph = GenericFactorGraph()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillOnServeVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()
  private val skillOnReturnVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

  private val tennisMatchFactors: ArrayBuffer[TennisMatchFactor] = ArrayBuffer[TennisMatchFactor]()

  private val lastVarId = new AtomicInteger()

  def addTournament(tournament: TournamentResult) {

    tournament.players.foreach { p =>
      //  getSkillTransitionVarId(p, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
      //  getSkillTransitionVarId(p, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)
    }

    tournament.matchResults.foreach { r =>

      getSkillTransitionVarId(r.player1, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
      getSkillTransitionVarId(r.player1, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)

      getSkillTransitionVarId(r.player2, skillOnServeVarIds, multiSkillParams.priorSkillOnServe, multiSkillParams.skillOnServeTransVariance)
      getSkillTransitionVarId(r.player2, skillOnReturnVarIds, multiSkillParams.priorSkillOnReturn, multiSkillParams.skillOnReturnTransVariance)

      val player1OnServeVarId = skillOnServeVarIds(r.player1).last
      val player1OnReturnVarId = skillOnReturnVarIds(r.player1).last

      val player2OnServeVarId = skillOnServeVarIds(r.player2).last
      val player2OnReturnVarId = skillOnReturnVarIds(r.player2).last

      val p1Points = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
      val p2Points = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
      val game = Game(tournament.tournamentTime, r.player1, r.player2, p1Points, p2Points)

      addTennisMatchToFactorGraph(player1OnServeVarId, player1OnReturnVarId,
        player2OnServeVarId, player2OnReturnVarId, game)
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
    player2OnServeVarId: Int, player2OnReturnVarId: Int, game: Game) {

    val player1VarId = lastVarId.getAndIncrement()
    val player2VarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    val player1Factor = PlayerFactor(player1OnServeVarId, player1OnReturnVarId, player1VarId)
    val player2Factor = PlayerFactor(player2OnServeVarId, player2OnReturnVarId, player2VarId)

    val matchFactor = new CachedTennisMatchFactor(player1Factor, player2Factor, outcomeVarId, multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn, game)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)

    tennisMatchFactors += matchFactor
  }

  def getSkillVarIdsOnServe(): mutable.Map[String, ArrayBuffer[Int]] = skillOnServeVarIds
  def getSkillVarIdsOnReturn(): mutable.Map[String, ArrayBuffer[Int]] = skillOnReturnVarIds

  def getFactorGraph(): FactorGraph = factorGraph

  def getTennisMatchFactors(): ArrayBuffer[TennisMatchFactor] = tennisMatchFactors

  def getPlayerSkill(player: String): PlayerSkills = {
    val ep = GenericEP(factorGraph)

    val varIdOnServe = skillOnServeVarIds(player).last
    val marginalOnServe = ep.marginal(varIdOnServe).asInstanceOf[GaussianFactor]
    val skillOnServe = PlayerSkill(marginalOnServe.m, marginalOnServe.v)

    val varIdOnReturn = skillOnReturnVarIds(player).last
    val marginalOnReturn = ep.marginal(varIdOnReturn).asInstanceOf[GaussianFactor]
    val skillOnReturn = PlayerSkill(marginalOnReturn.m, marginalOnReturn.v)

    PlayerSkills(player, new Date(0), skillOnServe, skillOnReturn)
  }

  def getSkills(): immutable.Map[String, PlayerSkills] = {
    val playerSkills = skillOnServeVarIds.keys.map(player => (player, getPlayerSkill(player))).toSeq

    immutable.Map(playerSkills: _*)
  }

  def pointProb(player1: String, player2: String): Tuple2[Double, Double] = {
    val pointModel = GenericPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

    val player1Skill = getPlayerSkill(player1)
    val player2Skill = getPlayerSkill(player2)

    var p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    var p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    (p1PointProb, p2PointProb)
  }

}