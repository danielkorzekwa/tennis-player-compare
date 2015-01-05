package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface._
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.NumOfSets

/**
 * Plot player skills over time
 */
object plotSimSkillsOverTimeApp extends App{

  val players = List(
    Player("Roger Federer", "Andy Murray", true, new Date(0), Surface.HARD,NumOfSets.THREE_SETS),
    Player("Roger Federer", "Andy Murray", true, new Date(0),  Surface.HARD,NumOfSets.FIVE_SETS))

  val skillsModelParams = getSimSkillsModelParams()
  plotSimPlayerSkillsOverTime(players, skillsModelParams.skillMeanFunc, skillsModelParams.skillCovFunc)

  private def getSimSkillsModelParams(): SkillsModelParams = {

    val (priorSkillOnServe, priorSkillOnReturn) = (4.65d, 0)
    def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) priorSkillOnServe else priorSkillOnReturn
    }

     val skillCovParams = Array(
    0.17156599472110903, 23.025850929940457, 23.025850929940457, 2.0716383109091745, 0.0, 1.9729749812915893, 0.5075553808229412, 0.4732727465613619
      )
      val skillCovFunc = SkillCovFunc(skillCovParams)
    val logPerfStdDev = 2.3

    val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)

    skillsModelParams
  }
}