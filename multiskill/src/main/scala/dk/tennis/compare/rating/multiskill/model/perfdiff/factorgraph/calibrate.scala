package dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph
import com.typesafe.scalalogging.slf4j.Logging
import scala.annotation.tailrec
import dk.bayes.math.linear.Matrix

object calibrate extends Logging {

  def apply(factorGraph: SkillsFactorGraph, threshold: Double = 1e-4, maxIter: Int = 10) {

    @tailrec
    def calibrate(playerSkillsMarginal: Matrix, iterNum: Int) {
      logger.debug("Calibrating factor graph:" + iterNum)
      if (iterNum > maxIter) logger.warn(s"Skills not converged in less than ${maxIter} iterations")

      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginalMean()
      if (iterNum >= maxIter || equals(newSkillsMarginal, playerSkillsMarginal, threshold)) return
      else calibrate(newSkillsMarginal, iterNum + 1)
    }

    calibrate(factorGraph.getPlayerSkillsPriorMean(), 1)

  }

  def equals(mean1: Matrix, mean2: Matrix, threshold: Double): Boolean = {
    mean1.matrix.isIdentical(mean2.matrix, threshold)
  }
}