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
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams

case class GenericMultiSkill(multiSkillParams: MultiSkillParams) extends MultiSkill {

  /** Map[playerName,player skills]*/
  private val skillsMap: mutable.Map[String, PlayerSkills] = mutable.Map()

  def processTennisMatch(matchResult: MatchResult) {

    val player1Skills = getSkill(matchResult.player1)
    val player2Skills = getSkill(matchResult.player2)

    val player1SkillsTrans = PlayerSkills(player1Skills.player,
      skillTransition(player1Skills.skillOnServe, multiSkillParams.skillOnServeTransVariance),
      skillTransition(player1Skills.skillOnReturn, multiSkillParams.skillOnReturnTransVariance))

    val player2SkillsTrans = PlayerSkills(player2Skills.player,
      skillTransition(player2Skills.skillOnServe, multiSkillParams.skillOnServeTransVariance),
      skillTransition(player2Skills.skillOnReturn, multiSkillParams.skillOnReturnTransVariance))

    val (newP1Skills, newP2Skills) = computeMarginals(player1SkillsTrans, player2SkillsTrans, matchResult.pointResults)

    skillsMap += matchResult.player1 -> newP1Skills
    skillsMap += matchResult.player2 -> newP2Skills

  }

  private def skillTransition(skill: PlayerSkill, skillTransVariance: Double): PlayerSkill = PlayerSkill(skill.mean, skill.variance + skillTransVariance)

  private def computeMarginals(player1Skills: PlayerSkills, player2Skill2: PlayerSkills, pointResults: Seq[PointResult]): Tuple2[PlayerSkills, PlayerSkills] = {

    val inPlayModel = GenericMatchModel(player1Skills, player2Skill2, multiSkillParams.perfVariance)

    pointResults.foreach(p => inPlayModel.onPoint(p))

    val newPlayer1Skills = inPlayModel.getP1Skills()
    val newPlayer2Skills = inPlayModel.getP2Skills()

    (newPlayer1Skills, newPlayer2Skills)

  }

  def getSkills(): immutable.Map[String, PlayerSkills] = skillsMap.toMap

  def getSkill(player: String): PlayerSkills = skillsMap.getOrElseUpdate(player,
    PlayerSkills(player, multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn))
}