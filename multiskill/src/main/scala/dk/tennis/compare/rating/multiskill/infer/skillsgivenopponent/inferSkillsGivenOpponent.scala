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
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik

object inferSkillsGivenOpponent {

  def apply(priorSkillsGivenOpponent: SkillsGivenOpponent, scores: Array[Score], skillMeanFunc: (Player) => Double,
    playerCovParams: Array[Double], playerCovFuncFactory: PlayerCovFuncFactory, logPerfStdDev: Double, iterNum: Int,
    progressListener: (SkillsGivenOpponent) => Unit = (skillsGivenOpponent) =>{}): SkillsGivenOpponent = {

    var currSkillsGivenOpponent = priorSkillsGivenOpponent

    for (i <- 0 until iterNum) {
      progressListener(currSkillsGivenOpponent)

      val skillsCovFunc = playerCovFuncFactory.create(playerCovParams, currSkillsGivenOpponent.skillsOnServeGivenOpponent, currSkillsGivenOpponent.skillsOnReturnGivenOpponent)
      val gp = GenericPerfDiffModel(skillMeanFunc, skillsCovFunc, logPerfStdDev, scores)
      gp.calibrateModel()

      val loglik = -OutcomeLik.totalLoglik(gp.inferPerfDiffs().map(p => p.perfDiff), scores)

      val currSkillsOnServeGivenOpponent = mStep(currSkillsGivenOpponent.skillsOnServeGivenOpponent, gp, skillsCovFunc, skillMeanFunc, true)
      val currSkillsOnReturnGivenOpponent = mStep(currSkillsGivenOpponent.skillsOnReturnGivenOpponent, gp, skillsCovFunc, skillMeanFunc, false)

      currSkillsGivenOpponent = SkillsGivenOpponent(currSkillsOnServeGivenOpponent, currSkillsOnReturnGivenOpponent)
    }

    currSkillsGivenOpponent
  }

//  private def mStep(oldSkillsGivenOpponent: Map[String, Seq[PlayerSkill]], gp: PerfDiffModel, skillsCovFunc: CovFunc, skillMeanFunc: (Player) => Double, playersOnServe: Boolean): Map[String, Seq[PlayerSkill]] = {
//
//    def getPlayerSkillsForPlayer(playerName: String): PlayerSkills = gp.calcPosteriorSkillsForPlayer(playerName, playersOnServe)
//    val skillInfer = CachedInferSkillGivenSkills(getPlayerSkillsForPlayer, skillsCovFunc, skillMeanFunc)
//
//    val newSkillsGivenOpponent = oldSkillsGivenOpponent.map {
//      case (player, skills) =>
//
//        val newSkills = skills.map(s => PlayerSkill(skillInfer.infer(s.player).m, s.player))
//        (player, newSkills)
//    }
//
//    newSkillsGivenOpponent
//  }
}