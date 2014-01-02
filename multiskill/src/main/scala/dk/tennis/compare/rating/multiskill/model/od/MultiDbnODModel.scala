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
import MultiDbnODModel._
import dk.bayes.model.factor.LinearGaussianFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import java.util.Date

case class MultiDbnODModel(priorSkillOnServe: PlayerSkill, priorSkillOnReturn: PlayerSkill, perfVariance: Double, directSkillVariance: Double) extends OffenceDefenceModel {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  private val factorGraph = GenericFactorGraph()

  private val lastVarId = new AtomicInteger()

  /**key - player name, value - skill var id*/
  private val skillOnServeVarIdMap: mutable.Map[String, DirectSkills] = mutable.Map()
  private val skillOnReturnVarIdMap: mutable.Map[String, DirectSkills] = mutable.Map()

  private val matchesByPlayer: mutable.Map[String, Int] = mutable.Map()

  def processGame(game: Game) {
    updateSkill(game.player1, game.player2, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(game.player1, game.player2, skillOnReturnVarIdMap, priorSkillOnReturn)

    updateSkill(game.player2, game.player1, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(game.player2, game.player1, skillOnReturnVarIdMap, priorSkillOnReturn)

    val player1OnServeVarId = skillOnServeVarIdMap(game.player1)
    val player1OnReturnVarId = skillOnReturnVarIdMap(game.player1)

    val player2OnServeVarId = skillOnServeVarIdMap(game.player2)
    val player2OnReturnVarId = skillOnReturnVarIdMap(game.player2)

    addTennisMatchToFactorGraph(player1OnServeVarId.directSkillsMap(game.player2), player1OnReturnVarId.directSkillsMap(game.player2),
      player2OnServeVarId.directSkillsMap(game.player1), player2OnReturnVarId.directSkillsMap(game.player1), game)

    matchesByPlayer += game.player1 -> (matchesByPlayer.getOrElse(game.player1, 0) + 1)
    matchesByPlayer += game.player2 -> (matchesByPlayer.getOrElse(game.player2, 0) + 1)

  }

  private def updateSkill(player: String, opponent: String, skillsVarIdMap: mutable.Map[String, DirectSkills], defaultSkill: PlayerSkill) {

    val directSkills = skillsVarIdMap.getOrElseUpdate(player,
      {
        val skillFactor = new GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
        factorGraph.addFactor(skillFactor)
        DirectSkills(skillFactor.varId, mutable.Map())
      })

    directSkills.directSkillsMap.getOrElseUpdate(opponent, {
      val directSkillFactor = new LinearGaussianFactor(directSkills.parentSkillVarId, lastVarId.getAndIncrement(), 1, 0, directSkillVariance)
      factorGraph.addFactor(directSkillFactor)
      directSkillFactor.varId
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
  def pointProb(game:Game):Tuple2[Double, Double] = {
    val pointModel = GenericPointModel(perfVariance, perfVariance)

    val player1Skill = getSkill(game.player1, game.player2)
    val player2Skill = getSkill(game.player2, game.player1)

    var p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    var p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    (p1PointProb, p2PointProb)
  }

  def getSkill(player: String, opponent: String): PlayerSkills = {
    updateSkill(player, opponent, skillOnServeVarIdMap, priorSkillOnServe)
    updateSkill(player, opponent, skillOnReturnVarIdMap, priorSkillOnReturn)

    val varIdOnServe = skillOnServeVarIdMap(player).directSkillsMap(opponent)
    val skillOnServe = skillMarginal(varIdOnServe)

    val varIdOnReturn = skillOnReturnVarIdMap(player).directSkillsMap(opponent)
    val skillOnReturn = skillMarginal(varIdOnReturn)

    PlayerSkills(player,  new Date(0),skillOnServe, skillOnReturn)
  }

  /**
   * @returns Map[playerName,playerSkills]
   */
  def getSkills(): immutable.Map[String, PlayerSkills] = throw new UnsupportedOperationException("Not implemented yet")

  /**@returns Avg skills (on serve,return)*/
  def getAvgSkills(player: String): Tuple2[PlayerSkill, PlayerSkill] = {
    val avgSkillOnServe = skillMarginal(skillOnServeVarIdMap(player).parentSkillVarId)
    val avgSkillOnReturn = skillMarginal(skillOnReturnVarIdMap(player).parentSkillVarId)

    (avgSkillOnServe, avgSkillOnReturn)
  }

  def getDirectSkillsOnServe(player: String): Map[String, PlayerSkill] = {
    val directSkillsOnServe = skillOnServeVarIdMap(player).directSkillsMap.map {
      case (opponent, varId) =>
        opponent -> getSkill(player, opponent).skillOnServe
    }

    directSkillsOnServe
  }

  def calibrate() {
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)
    val calibrateIterNum = epCalibrate.calibrate(1000, iterNum => {})
    logger.info(": EP calibration iterations: " + calibrateIterNum.iterNum)
  }

  private def skillMarginal(skillVarId: Int): PlayerSkill = {

    val ep = GenericEP(factorGraph)

    val marginalOnServe = ep.marginal(skillVarId).asInstanceOf[GaussianFactor]
    val skill = PlayerSkill(marginalOnServe.m, marginalOnServe.v)
    skill
  }

  def getMatchesNum(player: String) = matchesByPlayer.getOrElse(player, 0)
}

object MultiDbnODModel {

  /**
   * @param parentSkillVarId
   * @param directSkillsMap  key - opponent , value - directSkillVarId
   */
  case class DirectSkills(parentSkillVarId: Int, directSkillsMap: mutable.Map[String, Int])
}