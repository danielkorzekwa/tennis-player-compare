package dk.tennis.compare.rating.multiskill.learn

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.inferSkillsGivenOpponent
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.SkillsGivenOpponent
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

object calcSkillsModelParams extends Logging {

  def apply(priorSkillParams: SkillsModelParams, skillsCovFactory: PlayerCovFuncFactory, scores: Array[Score], gradientMask: Array[Double],
    progressListener: (SkillsModelParams, Boolean) => Unit = (state, covParamsLearn) => {}, iterNum: Int): SkillsModelParams = {

    var currSkillParams = priorSkillParams
    for (i <- 0 until iterNum) {

      logger.info("======================================================")
      logger.info("Learning cov params")
      logger.info("======================================================")

      def diffFuncProgressListener(state: SkillDiffFuncState) {
        progressListener(currSkillParams.copy(skillCovFunc = state.skillCovFunc), true)
      }

      val diffFunction = SkillsDiffFunction(scores,currSkillParams.skillMeanFunc,
        Some(gradientMask), diffFuncProgressListener, currSkillParams.skillCovFunc)

      val optimizer = new LBFGS[DenseVector[Double]](maxIter = 1, m = 6, tolerance = 1.0E-9)
      val optIters = optimizer.iterations(diffFunction, DenseVector(currSkillParams.skillCovFunc.getParams().toArray)).toList
      val newParams = optIters.last.x

      val newSkillCovFunc = currSkillParams.skillCovFunc.withParams(newParams.data)
      currSkillParams = SkillsModelParams(diffFunction.currSkillPriorMeanFunc, newSkillCovFunc)
      progressListener(currSkillParams, false)

      logger.info("======================================================")
      logger.info("Learning skills given opponent")
      logger.info("======================================================")

      //  val posteriorSkillsGivenOpponent = inferSkillsGivenOpponent(currSkillParams.skillsGivenOpponent, scores, currSkillParams.skillMeanFunc,
      //    newParams.data.dropRight(1), skillsCovFactory, logPerfStdDev = newParams.data.last, iterNum = 1)

      def getPlayerSkill(player: Player): PlayerSkill = throw new UnsupportedOperationException("Not implemented yet")

      val newSkillCovFunc2 = skillsCovFactory.create(currSkillParams.skillCovFunc.getParams, getPlayerSkill)
      currSkillParams = SkillsModelParams(currSkillParams.skillMeanFunc, newSkillCovFunc2)
    }

    currSkillParams

  }
}