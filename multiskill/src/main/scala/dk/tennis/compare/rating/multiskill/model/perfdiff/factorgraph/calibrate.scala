package dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph
import com.typesafe.scalalogging.slf4j.Logging
import scala.annotation.tailrec
import dk.bayes.math.linear.Matrix

object calibrate extends Logging {

  def apply(factorGraph: SkillsFactorGraph, threshold: Double = 1e-4) {

    @tailrec
    def calibrate(playerSkillsMarginal: Matrix) {
      logger.info("Calibrating tournament model...")
      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginalMean()
      val newGameToSkillsFactorMsgs = factorGraph.gameToSkillsFactorMsgs

      //if (equals(newGameToSkillsFactorMsgs, gameToSkillsFactorMsgs, threshold)) return
      if (equals(newSkillsMarginal, playerSkillsMarginal, threshold)) return
      else calibrate(newSkillsMarginal)
    }

    calibrate(factorGraph.getPlayerSkillsMarginalMean)

  }

  def equals(mean1: Matrix, mean2: Matrix, threshold: Double): Boolean = {
    mean1.matrix.isIdentical(mean2.matrix, threshold)
  }
}