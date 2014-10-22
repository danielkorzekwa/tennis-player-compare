package dk.tennis.compare.rating.multiskill.learn

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score

object calcSkillsModelParams {

  def apply(priorSkillParams: SkillsModelParams, skillsCovFactory: PlayerCovFuncFactory, scores: Array[Score],gradientMask:Array[Double],
      progressListener: (SkillDiffFuncState) => Unit = (state) => {}): SkillsModelParams = {

    val diffFunction = SkillsDiffFunction(scores,
      priorSkillParams.skillPriorMeanOnServe, priorSkillParams.skillPriorMeanOnReturn,
      priorSkillParams.skillsOnServeGivenOpponent, priorSkillParams.skillsOnReturnGivenOpponent, skillsCovFactory,
      Some(gradientMask),progressListener)

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-9)
    val optIters = optimizer.iterations(diffFunction, DenseVector(priorSkillParams.skillCovParams)).toList
    val newParams = optIters.last.x

    SkillsModelParams(diffFunction.currSkillPriorMeanOnServe, diffFunction.currSkillPriorMeanOnReturn,
      newParams.data,
     diffFunction.currSkillsOnServeGivenOpponent, diffFunction.currSkillsOnReturnGivenOpponent)

  }
}