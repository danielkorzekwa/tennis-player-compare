package dk.tennis.compare.rating.multiskill.learn

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.inferSkillsGivenOpponent
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.SkillsGivenOpponent
import com.typesafe.scalalogging.slf4j.Logging

object calcSkillsModelParams extends Logging {

  def apply(priorSkillParams: SkillsModelParams, skillsCovFactory: PlayerCovFuncFactory, scores: Array[Score], gradientMask: Array[Double],
    progressListener: (SkillsModelParams,Boolean) => Unit = (state,covParamsLearn) => {}, iterNum: Int): SkillsModelParams = {

    var currSkillParams = priorSkillParams
    for (i <- 0 until iterNum) {

      logger.info("======================================================")
      logger.info("Learning cov params")
      logger.info("======================================================")

      progressListener(currSkillParams,false)

      def diffFuncProgressListener(state: SkillDiffFuncState) {
        progressListener(currSkillParams.copy(skillCovParams = state.params.data),true)
      }
      val diffFunction = SkillsDiffFunction(scores,
        currSkillParams.skillMeanFunc,
        currSkillParams.skillsGivenOpponent, skillsCovFactory,
        Some(gradientMask), diffFuncProgressListener)

      val optimizer = new LBFGS[DenseVector[Double]](maxIter = 1, m = 6, tolerance = 1.0E-9)
      val optIters = optimizer.iterations(diffFunction, DenseVector(currSkillParams.skillCovParams)).toList
      val newParams = optIters.last.x

      logger.info("======================================================")
      logger.info("Learning skills given opponent")
      logger.info("======================================================")

      val posteriorSkillsGivenOpponent = inferSkillsGivenOpponent(currSkillParams.skillsGivenOpponent, scores, currSkillParams.skillMeanFunc,
        currSkillParams.skillCovParams.dropRight(1), skillsCovFactory, logPerfStdDev = currSkillParams.skillCovParams.last, iterNum = 3)

      currSkillParams = SkillsModelParams(diffFunction.currSkillPriorMeanFunc, newParams.data, posteriorSkillsGivenOpponent)
    }

    currSkillParams

  }
}