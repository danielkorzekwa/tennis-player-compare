package dk.tennis.compare.rating.multiskill.model.perfdiff.math

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff

object gameSkillsToPerfDiffs {

  def apply(gameSkills: Seq[MultivariateGaussian], logPerfStdDev: Double): Seq[PerfDiff] = {

    val perfDiffToOutcomeMsgs = gameSkills.map { gameSkill =>
      val gamePerfDiff = gameSkillToPerfDiff(gameSkill, logPerfStdDev)

      gamePerfDiff

    }

    perfDiffToOutcomeMsgs
  }
}