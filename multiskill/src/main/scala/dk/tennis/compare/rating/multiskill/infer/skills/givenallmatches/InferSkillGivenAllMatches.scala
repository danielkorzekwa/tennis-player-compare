package dk.tennis.compare.rating.multiskill.infer.skills.givenallmatches

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill

case class InferSkillGivenAllMatches(skillsModelParams:SkillsModelParams,logPerfStdDev:Double,scores: Array[Score]) extends InferSkill {

   val infer = InferSkillGivenPlayer(skillsModelParams, logPerfStdDev, scores)

  def inferSkill(player: Player): PlayerSkill = {
    val skill = infer.inferSkill(player)
    skill
  }

}