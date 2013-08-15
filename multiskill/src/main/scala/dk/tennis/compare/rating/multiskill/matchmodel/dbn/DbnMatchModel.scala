package dk.tennis.compare.rating.multiskill.matchmodel.dbn

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.bayes.model.factor.BivariateGaussianFactor

trait DbnMatchModel {

  /**
   * @param maxIter Maximum number of iteration for the match model to converge
   *
   * @param Actual number of iterations
   */
  def calibrate(maxIter: Int): Int

  def getP1Skills(): PlayerSkills
  def getP2Skills(): PlayerSkills

  def getPerfVarOnServe(): IndexedSeq[BivariateGaussianFactor]
  def getPerfVarOnReturn(): IndexedSeq[BivariateGaussianFactor]
}