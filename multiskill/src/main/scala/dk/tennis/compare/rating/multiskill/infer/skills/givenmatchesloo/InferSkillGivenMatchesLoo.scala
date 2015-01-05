package dk.tennis.compare.rating.multiskill.infer.skills.givenmatchesloo

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

case class InferSkillGivenMatchesLoo(matchResults: Seq[MatchResult]) {

  private val skillCovParams = Array(
    2.022132402833459, // opponent 
    2.335108140472381, 0.8585245019924235, 0.6068550430203135, // surface
    -0.7964151980207621, 3.0080055487894057, 0.422376471909165, 7.932430851981252 //time
    )

  private val (priorSkillOnServe, priorSkillOnReturn) = (4.65d, 0)
  private val logPerfStdDev = 2.3

  val skillCovFunc = SkillCovFunc(skillCovParams)
  val allScores = Score.toScores(matchResults)
  val infer = GenericPerfDiffModel(playerSkillMeanPrior, skillCovFunc, logPerfStdDev, allScores)
  infer.calibrateModel()

  val perfDiffs = infer.inferPerfDiffs()

  /**
   * @param player - player on serve only is supported
   */
  def inferSkill(player: Player): PlayerSkill = {

    val skill = allScores.zip(perfDiffs).filter { case (score, perfDiff) => score.player1.equals(player) }.map { case (score, perfDiff) => PlayerSkill(perfDiff.getP1Skill, player) }
    require(skill.size == 1)
    skill.head
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}