package dk.tennis.compare.rating.multiskill.model.od

import scala.collection._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import java.util.Date
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.model.od.Game

case class GenericOffenceDefenceModel(multiSkillParams: MultiSkillParams) extends OffenceDefenceModel {

  /** Map[playerName,player skills]*/
  private val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map()

  private val lastGameTimeByPlayer: mutable.Map[String, Date] = mutable.Map()

  private val matchesByPlayer: mutable.Map[String, Int] = mutable.Map()

  def processGame(game: Game) {

    val player1SkillsTrans = playerSkillsTransition(game.player1, game.player2, game.gameTime)
    val player2SkillsTrans = playerSkillsTransition(game.player2, game.player1, game.gameTime)

    val (newP1Skills, newP2Skills) = computeMarginals(player1SkillsTrans, player2SkillsTrans, game.p1PointsOnOffence, game.p2PointsOnOffence)

    skillsMap += game.player1 -> newP1Skills
    skillsMap += game.player2 -> newP2Skills

    matchesByPlayer += game.player1 -> (matchesByPlayer.getOrElse(game.player1, 0)+1)
    matchesByPlayer += game.player2 -> (matchesByPlayer.getOrElse(game.player2, 0)+1)

  }
  
 
  private def playerSkillsTransition(player: String, opponent: String, gameTime: Date): PlayerSkills = {

    val playerSkills = getSkill(player, opponent)

    val transPlayerSkills = if (lastGameTimeByPlayer.getOrElse(player, new Date(0)).getTime < gameTime.getTime) {

      lastGameTimeByPlayer += player -> gameTime
      PlayerSkills(playerSkills.player,
        skillTransition(playerSkills.skillOnServe, multiSkillParams.skillOnServeTransVariance),
        skillTransition(playerSkills.skillOnReturn, multiSkillParams.skillOnReturnTransVariance))

    } else playerSkills

    transPlayerSkills
  }
  private def skillTransition(skill: PlayerSkill, skillTransVariance: Double): PlayerSkill = PlayerSkill(skill.mean, skill.variance + skillTransVariance)

  private def computeMarginals(player1Skills: PlayerSkills, player2Skills: PlayerSkills,
    p1PointsOnOffence: Tuple2[Int, Int], p2PointsOnOffence: Tuple2[Int, Int]): Tuple2[PlayerSkills, PlayerSkills] = {

    val multiPointModel = GenericMultiPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

    val p1PointsRatio = p1PointsOnOffence._1.toDouble / p1PointsOnOffence._2
    val p2PointsRatio = p2PointsOnOffence._1.toDouble / p2PointsOnOffence._2

    val (newP1SkillOnServe, newP2SkillOnReturn) =
      multiPointModel.skillMarginals(player1Skills.skillOnServe, player2Skills.skillOnReturn, p1PointsOnOffence._1, p1PointsOnOffence._2)

    val (newP2SkillOnServe, newP1SkillOnReturn) =
      multiPointModel.skillMarginals(player2Skills.skillOnServe, player1Skills.skillOnReturn, p2PointsOnOffence._1, p2PointsOnOffence._2)

    val newPlayer1Skills = PlayerSkills(player1Skills.player, newP1SkillOnServe, newP1SkillOnReturn)
    val newPlayer2Skills = PlayerSkills(player2Skills.player, newP2SkillOnServe, newP2SkillOnReturn)

    (newPlayer1Skills, newPlayer2Skills)

  }

  def getSkill(player: String, opponent: String): PlayerSkills = skillsMap.getOrElseUpdate(player,
    PlayerSkills(player, multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn))

  def getSkills() = skillsMap.toMap

  def pointProb(player1: String, player2: String): Tuple2[Double, Double] = {
    val pointModel = GenericPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

    val player1Skill = getSkill(player1, player2)
    val player2Skill = getSkill(player2, player1)

    var p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    var p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    (p1PointProb, p2PointProb)
  }
  
  def getMatchesNum(player:String) = matchesByPlayer.getOrElse(player,0)
}