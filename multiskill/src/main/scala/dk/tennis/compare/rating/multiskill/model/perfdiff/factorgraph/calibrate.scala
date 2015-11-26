package dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph

import scala.annotation.tailrec

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.DenseVector
import dk.bayes.math.linear.isIdentical

object calibrate extends LazyLogging {

  def apply(factorGraph: SkillsFactorGraph, threshold: Double = 1e-4, maxIter: Int = 10) {

    @tailrec
    def calibrate(playerSkillsMarginal: DenseVector[Double], iterNum: Int) {
      logger.debug("Calibrating factor graph:" + iterNum)
      if (iterNum > maxIter) logger.warn(s"Skills not converged in less than ${maxIter} iterations")

      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginalMean()
      if (iterNum >= maxIter || equals(newSkillsMarginal, playerSkillsMarginal, threshold)) return
      else calibrate(newSkillsMarginal, iterNum + 1)
    }

    calibrate(factorGraph.getPlayerSkillsPriorMean(), 1)

  }

  def equals(mean1: DenseVector[Double], mean2: DenseVector[Double], tol: Double): Boolean = {
   isIdentical(mean1,mean2,tol)
  }
}