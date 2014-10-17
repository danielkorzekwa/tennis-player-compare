package dk.tennis.compare.rating.multiskill.learn

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score

object calcSkillsModelParams {

  def apply(priorSkillParams: SkillsModelParams, skillsCovFactory: PlayerCovFuncFactory, scores: Array[Score]): SkillsModelParams = {

    val gradientMask = priorSkillParams.skillCovParams.map(v => 1d)
    gradientMask(gradientMask.size - 1) = 0 //do not learn performance variance
    val diffFunction = SkillsDiffFunction(scores,
      priorSkillParams.skillPriorMeanOnServe, priorSkillParams.skillPriorMeanOnReturn,
      priorSkillParams.skillsOnServeGivenOpponent, priorSkillParams.skillsOnReturnGivenOpponent, skillsCovFactory,
      Some(gradientMask))

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-9)
    val optIters = optimizer.iterations(diffFunction, DenseVector(priorSkillParams.skillCovParams)).toList
    val newParams = optIters.last.x

    SkillsModelParams(diffFunction.currSkillPriorMeanOnServe, diffFunction.currSkillPriorMeanOnReturn,
      newParams.data,
     diffFunction.currSkillsOnServeGivenOpponent, diffFunction.currSkillsOnReturnGivenOpponent)

  }
}