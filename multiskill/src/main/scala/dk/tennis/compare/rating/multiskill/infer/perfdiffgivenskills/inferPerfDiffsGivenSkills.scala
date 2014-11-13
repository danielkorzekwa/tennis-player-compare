package dk.tennis.compare.rating.multiskill.infer.perfdiffgivenskills

import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff

object inferPerfDiffsGivenSkills {

   def apply(gameSkills: Seq[MultivariateGaussian], logPerfStdDev: Double): Seq[PerfDiff] = {

    val perfDiffToOutcomeMsgs = gameSkills.map { gameSkill =>
      val gamePerfDiff = inferPerfDiffGivenSkills(gameSkill, logPerfStdDev)

      gamePerfDiff

    }

    perfDiffToOutcomeMsgs
  }
  
}