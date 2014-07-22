package dk.tennis.compare.rating.multiskill.model.gpskill.naive.factorgraph

import dk.bayes.math.gaussian.MultivariateGaussian
import com.typesafe.scalalogging.slf4j.Logging
import scala.annotation.tailrec

object calibrate extends Logging {

  def apply(factorGraph: GPSkillsFactorGraph, threshold: Double = 1e-4) {

    @tailrec
    def calibrate(playerSkillsMarginal: MultivariateGaussian) {
      logger.info("Calibrating tournament model...")
      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginal()

      if (equals(newSkillsMarginal, playerSkillsMarginal, threshold)) return
      else calibrate(newSkillsMarginal)
    }

    calibrate(factorGraph.priorPlayerSkills)

  }

  def equals(gaussian1: MultivariateGaussian, gaussian2: MultivariateGaussian, threshold: Double): Boolean = {

    val (mean1, variance1) = (gaussian1.m, gaussian1.v)
    val (mean2, variance2) = (gaussian2.m, gaussian2.v)

    mean1.matrix.isIdentical(mean2.matrix, threshold)
    // variance1.matrix.isIdentical(variance2.matrix, threshold)

  }
}