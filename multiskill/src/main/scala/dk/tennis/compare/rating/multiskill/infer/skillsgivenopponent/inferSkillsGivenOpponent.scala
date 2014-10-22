package dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.infer.skillgivenskills.CachedInferSkillGivenSkills
import dk.tennis.compare.rating.multiskill.learn.PlayerCovFuncFactory
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel

object inferSkillsGivenOpponent {

  def apply(priorSkillsGivenOpponent: SkillsGivenOpponent, scores: Array[Score], skillMeanFunc: (Player) => Double, playerCovParams: Array[Double], playerCovFuncFactory: PlayerCovFuncFactory, logPerfStdDev: Double): SkillsGivenOpponent = {

    val skillsCovFunc = playerCovFuncFactory.create(playerCovParams, priorSkillsGivenOpponent.skillsOnServeGivenOpponent, priorSkillsGivenOpponent.skillsOnReturnGivenOpponent)
    val gp = GenericPerfDiffModel(skillMeanFunc, skillsCovFunc, logPerfStdDev, scores)
    gp.calibrateModel()

    //learn skills given opponent
    val newSkillsOnServeGivenOpponent = mStep(priorSkillsGivenOpponent.skillsOnServeGivenOpponent, gp, skillsCovFunc, skillMeanFunc, true)
    val newSkillsOnReturnGivenOpponent = mStep(priorSkillsGivenOpponent.skillsOnReturnGivenOpponent, gp, skillsCovFunc, skillMeanFunc, false)

    SkillsGivenOpponent(newSkillsOnServeGivenOpponent, newSkillsOnReturnGivenOpponent)
  }

  private def mStep(oldSkillsGivenOpponent: Map[String, Seq[PlayerSkill]], gp: PerfDiffModel, skillsCovFunc: CovFunc, skillMeanFunc: (Player) => Double, playersOnServe: Boolean): Map[String, Seq[PlayerSkill]] = {

    def getPlayerSkillsForPlayer(playerName: String): PlayerSkills = gp.calcPosteriorSkillsForPlayer(playerName, playersOnServe)
    val skillInfer = CachedInferSkillGivenSkills(getPlayerSkillsForPlayer, skillsCovFunc, skillMeanFunc)

    val newSkillsGivenOpponent = oldSkillsGivenOpponent.map {
      case (player, skills) =>

        val newSkills = skills.map(s => PlayerSkill(skillInfer.infer(s.player).m, s.player))
        (player, newSkills)
    }

    newSkillsGivenOpponent
  }
}