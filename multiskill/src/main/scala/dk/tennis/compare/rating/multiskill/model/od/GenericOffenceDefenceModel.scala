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

case class GenericOffenceDefenceModel(multiSkillParams: MultiSkillParams) extends OffenceDefenceModel {

  /** Map[playerName,player skills]*/
  private val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map()

  private val matchesByPlayer: mutable.Map[String, Int] = mutable.Map()

  def processGame(game: Game) {

    val player1SkillsTrans = playerSkillsTransition(game.player1, game.player2, game.gameTime)
    val player2SkillsTrans = playerSkillsTransition(game.player2, game.player1, game.gameTime)

    val (newP1Skills, newP2Skills) = computeMarginals(player1SkillsTrans, player2SkillsTrans, game.p1PointsOnOffence, game.p2PointsOnOffence)

    skillsMap += game.player1 -> newP1Skills
    skillsMap += game.player2 -> newP2Skills

    matchesByPlayer += game.player1 -> (matchesByPlayer.getOrElse(game.player1, 0) + 1)
    matchesByPlayer += game.player2 -> (matchesByPlayer.getOrElse(game.player2, 0) + 1)

  }

  private def playerSkillsTransition(player: String, opponent: String, gameTime: Date): PlayerSkills = {

    val playerSkills = getSkill(player, opponent)

    val transPlayerSkills =
      PlayerSkills(playerSkills.player, new Date(0),
        skillTransition(playerSkills.skillOnServe, multiSkillParams.skillOnServeTransVariance),
        skillTransition(playerSkills.skillOnReturn, multiSkillParams.skillOnReturnTransVariance))

    transPlayerSkills
  }
  private def skillTransition(skill: PlayerSkill, skillTransVariance: Double): PlayerSkill = PlayerSkill(skill.mean, skill.variance + skillTransVariance)

  private def computeMarginals(player1Skills: PlayerSkills, player2Skills: PlayerSkills,
    p1PointsOnOffence: Tuple2[Int, Int], p2PointsOnOffence: Tuple2[Int, Int]): Tuple2[PlayerSkills, PlayerSkills] = {

    val multiPointModel = GenericMultiPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

    val (newP1SkillOnServe, newP2SkillOnReturn, _) =
      multiPointModel.skillMarginals(player1Skills.skillOnServe, player2Skills.skillOnReturn, p1PointsOnOffence._1, p1PointsOnOffence._2)

    val (newP2SkillOnServe, newP1SkillOnReturn, _) =
      multiPointModel.skillMarginals(player2Skills.skillOnServe, player1Skills.skillOnReturn, p2PointsOnOffence._1, p2PointsOnOffence._2)

    val newPlayer1Skills = PlayerSkills(player1Skills.player,  new Date(0),newP1SkillOnServe, newP1SkillOnReturn)
    val newPlayer2Skills = PlayerSkills(player2Skills.player,  new Date(0),newP2SkillOnServe, newP2SkillOnReturn)

    (newPlayer1Skills, newPlayer2Skills)

  }

  def getSkill(player: String, opponent: String): PlayerSkills = skillsMap.getOrElseUpdate(player,
    PlayerSkills(player, new Date(0), multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn))

  def getSkills() = skillsMap.toMap

  def pointProb(game:Game):Tuple2[Double, Double] = {
    val pointModel = GenericPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

    val player1Skill = getSkill(game.player1, game.player2)
    val player2Skill = getSkill(game.player2, game.player1)

    //  val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    //  val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    val p1PointProb = pointModel.pointProb(skillTransition(player1Skill.skillOnServe, multiSkillParams.skillOnServeTransVariance + 00), skillTransition(player2Skill.skillOnReturn, multiSkillParams.skillOnServeTransVariance + 00))
    val p2PointProb = pointModel.pointProb(skillTransition(player2Skill.skillOnServe, multiSkillParams.skillOnServeTransVariance + 00), skillTransition(player1Skill.skillOnReturn, multiSkillParams.skillOnServeTransVariance + 00))

    (p1PointProb, p2PointProb)
  }

  def getMatchesNum(player: String) = matchesByPlayer.getOrElse(player, 0)
}