package dk.tennis.compare.rating.multiskill.infer.skillgivenplayer

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import scala.collection._
import dk.tennis.compare.rating.multiskill.infer.skillgivenskills.InferSkillGivenSkills
import scala.collection.mutable.HashMap
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score

case class InferSkillGivenPlayer(skillsModelParams: SkillsModelParams, logPerfStdDev: Double, scores: Array[Score]) {

  private val inferByPlayerMap: mutable.Map[PlayerKey, InferSkillGivenSkills] = new HashMap()

  private val gp = GenericPerfDiffModel(skillsModelParams.skillMeanFunc, skillsModelParams.skillCovFunc, logPerfStdDev, scores)
  gp.calibrateModel()

  def inferSkill(player: Player): PlayerSkill = {

    val playerKey = PlayerKey(player.playerName, player.onServe)
    val infer = inferByPlayerMap.getOrElseUpdate(playerKey, getInfer(playerKey))
    val skill = infer.infer(player)
    PlayerSkill(skill, player)
  }

  private def getInfer(playerKey: PlayerKey): InferSkillGivenSkills = {
    val playerSkills = gp.calcPosteriorSkillsForPlayer(playerKey.playerName, playerKey.onServe)
    val infer = InferSkillGivenSkills(playerSkills, skillsModelParams.skillCovFunc, skillsModelParams.skillMeanFunc)
    infer
  }
}