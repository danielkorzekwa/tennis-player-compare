package dk.tennis.compare.rating.multiskill.model.od

import scala.collection._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import java.util.Date
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.domain.MatchResult

case class GenericOffenceDefenceModel(multiSkillParams: MultiSkillParams) extends OffenceDefenceModel {

  /** Map[playerName,player skills]*/
  private val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map()

  private val lastGameTimeByPlayer: mutable.Map[String, Date] = mutable.Map()

  def processGame(gameTime: Date, player1: String, player2: String, p1PointsOnOffence: Tuple2[Int, Int], p2PointsOnOffence: Tuple2[Int, Int]) {

    val player1SkillsTrans = playerSkillsTransition(player1, gameTime)
    val player2SkillsTrans = playerSkillsTransition(player2, gameTime)

    val (newP1Skills, newP2Skills) = computeMarginals(player1SkillsTrans, player2SkillsTrans, p1PointsOnOffence, p2PointsOnOffence)

    skillsMap += player1 -> newP1Skills
    skillsMap += player2 -> newP2Skills

  }

  private def playerSkillsTransition(player: String, gameTime: Date): PlayerSkills = {

    val playerSkills = getSkill(player)

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

    val (newP1SkillOnServe, newP2SkillOnReturn) =
      multiPointModel.skillMarginals(player1Skills.skillOnServe, player2Skills.skillOnReturn, p1PointsOnOffence._1, p1PointsOnOffence._2)

    val (newP2SkillOnServe, newP1SkillOnReturn) =
      multiPointModel.skillMarginals(player2Skills.skillOnServe, player1Skills.skillOnReturn, p2PointsOnOffence._1, p2PointsOnOffence._2)

    val newPlayer1Skills = PlayerSkills(player1Skills.player, newP1SkillOnServe, newP1SkillOnReturn)
    val newPlayer2Skills = PlayerSkills(player2Skills.player, newP2SkillOnServe, newP2SkillOnReturn)

    (newPlayer1Skills, newPlayer2Skills)

  }

  def getSkill(player: String): PlayerSkills = skillsMap.getOrElseUpdate(player,
    PlayerSkills(player, multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn))
}