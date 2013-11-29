package dk.tennis.compare.rating.multiskill.model.od

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.bayes.model.factorgraph.GenericFactorGraph
import scala.collection._
import dk.bayes.model.factor.GaussianFactor
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.CachedTennisMatchFactor
import java.util.regex.MatchResult
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.infer.ep.GenericEP
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel

case class DbnODModel(priorSkillOnServe: PlayerSkill, priorSkillOnReturn: PlayerSkill, perfVariance: Double) extends OffenceDefenceModel {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  private val factorGraph = GenericFactorGraph()

  private val lastVarId = new AtomicInteger()

  /**key - player name, value - skill var id*/
  private val skillOnServeVarIdMap: mutable.Map[String, Int] = mutable.Map()
  private val skillOnReturnVarIdMap: mutable.Map[String, Int] = mutable.Map()

  private val matchesByPlayer: mutable.Map[String, Int] = mutable.Map()

  def processGame(game: Game) {
    updateSkill(game.player1, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(game.player1, skillOnReturnVarIdMap, priorSkillOnReturn)

    updateSkill(game.player2, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(game.player2, skillOnReturnVarIdMap, priorSkillOnReturn)

    val player1OnServeVarId = skillOnServeVarIdMap(game.player1)
    val player1OnReturnVarId = skillOnReturnVarIdMap(game.player1)

    val player2OnServeVarId = skillOnServeVarIdMap(game.player2)
    val player2OnReturnVarId = skillOnReturnVarIdMap(game.player2)

    addTennisMatchToFactorGraph(player1OnServeVarId, player1OnReturnVarId,
      player2OnServeVarId, player2OnReturnVarId, game)

    matchesByPlayer += game.player1 -> (matchesByPlayer.getOrElse(game.player1, 0) + 1)
    matchesByPlayer += game.player2 -> (matchesByPlayer.getOrElse(game.player2, 0) + 1)

  }

  private def updateSkill(player: String, skillsVarIdMap: mutable.Map[String, Int], defaultSkill: PlayerSkill) {

    skillsVarIdMap.getOrElseUpdate(player,
      {
        val skillFactor = new GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
        factorGraph.addFactor(skillFactor)
        skillFactor.varId
      })

  }

  private def addTennisMatchToFactorGraph(player1OnServeVarId: Int, player1OnReturnVarId: Int,
    player2OnServeVarId: Int, player2OnReturnVarId: Int, game: Game) {

    val player1VarId = lastVarId.getAndIncrement()
    val player2VarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    val player1Factor = PlayerFactor(player1OnServeVarId, player1OnReturnVarId, player1VarId)
    val player2Factor = PlayerFactor(player2OnServeVarId, player2OnReturnVarId, player2VarId)

    val matchFactor = new CachedTennisMatchFactor(player1Factor, player2Factor, outcomeVarId, perfVariance, perfVariance, game)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)

  }

  /**
   * Returns the probability of winning a point. [player1ProbOnOffence,player2ProbOnOffence]
   */
  def pointProb(player1: String, player2: String): Tuple2[Double, Double] = {
    val pointModel = GenericPointModel(perfVariance, perfVariance)

    val player1Skill = getSkill(player1, player2)
    val player2Skill = getSkill(player2, player1)

    var p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    var p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    (p1PointProb, p2PointProb)
  }

  def getSkill(player: String, opponent: String): PlayerSkills = {
    updateSkill(player, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(player, skillOnReturnVarIdMap, priorSkillOnReturn)

    val ep = GenericEP(factorGraph)

    val varIdOnServe = skillOnServeVarIdMap(player)
    val marginalOnServe = ep.marginal(varIdOnServe).asInstanceOf[GaussianFactor]
    val skillOnServe = PlayerSkill(marginalOnServe.m, marginalOnServe.v)

    val varIdOnReturn = skillOnReturnVarIdMap(player)
    val marginalOnReturn = ep.marginal(varIdOnReturn).asInstanceOf[GaussianFactor]
    val skillOnReturn = PlayerSkill(marginalOnReturn.m, marginalOnReturn.v)

    PlayerSkills(player, skillOnServe, skillOnReturn)
  }

  def calibrate() {
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)
    val calibrateIterNum = epCalibrate.calibrate(1000, iterNum => {})
    logger.info(": EP calibration iterations: " + calibrateIterNum.iterNum)
  }

  /**
   * @returns Map[playerName,playerSkills]
   */
  def getSkills(): immutable.Map[String, PlayerSkills] = throw new UnsupportedOperationException("Not implemented yet")

  def getMatchesNum(player: String) = matchesByPlayer.getOrElse(player, 0)

}