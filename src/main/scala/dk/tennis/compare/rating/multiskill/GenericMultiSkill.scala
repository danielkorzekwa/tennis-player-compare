package dk.tennis.compare.rating.multiskill
import scala.collection._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel

case class GenericMultiSkill(skillTransVariance: Double, perfVariance: Double) extends MultiSkill {

  /** Map[playerName,player skills]*/
  private val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map()

  private val defaultSkill = PlayerSkill(0, 1)

  def processTennisMatch(player1: String, player2: String, pointResults: Seq[PointResult]) {

    val player1Skills = skillsMap.getOrElseUpdate(player1, PlayerSkills(player1, defaultSkill, defaultSkill))
    val player2Skills = skillsMap.getOrElseUpdate(player2, PlayerSkills(player2, defaultSkill, defaultSkill))

    val player1SkillsTrans = PlayerSkills(player1Skills.player, skillTransition(player1Skills.skillOnServe), skillTransition(player1Skills.skillOnReturn))
    val player2SkillsTrans = PlayerSkills(player2Skills.player, skillTransition(player2Skills.skillOnServe), skillTransition(player2Skills.skillOnReturn))

    val (newP1SkillOnServe, newP2SkillOnReturn) = computeMarginals(player1SkillsTrans, player2SkillsTrans, pointResults)

    skillsMap += player1 -> newP1SkillOnServe
    skillsMap += player2 -> newP2SkillOnReturn

  }

  private def skillTransition(skill: PlayerSkill): PlayerSkill = PlayerSkill(skill.mean, skill.variance + skillTransVariance)

  private def computeMarginals(player1Skills: PlayerSkills, player2Skill2: PlayerSkills, pointResults: Seq[PointResult]): Tuple2[PlayerSkills, PlayerSkills] = {

    val inPlayModel = GenericMatchModel(player1Skills, player2Skill2, perfVariance)

    pointResults.foreach(p => inPlayModel.onPoint(p))

    val newPlayer1Skills = inPlayModel.getP1Skills()
    val newPlayer2Skills = inPlayModel.getP2Skills()

    (newPlayer1Skills, newPlayer2Skills)

  }

  def getSkills(): immutable.Map[String, PlayerSkills] = skillsMap.toMap
}