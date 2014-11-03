package dk.tennis.compare.rating.multiskill.infer.skillmodelparams

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer

object calcSkillsModelParams extends Logging {

  def apply(priorSkillParams: SkillsModelParams, scores: Array[Score], gradientMask: Array[Double],
    progressListener: (SkillsModelParams, Boolean) => Unit = (state, covParamsLearn) => {}, iterNum: Int, logPerfStdDev: Double): SkillsModelParams = {

    var currSkillParams = priorSkillParams
    for (i <- 0 until iterNum) {

      logger.info("======================================================")
      logger.info("Learning cov params")
      logger.info("======================================================")

      def diffFuncProgressListener(state: SkillDiffFuncState) {
        progressListener(currSkillParams.copy(skillCovFunc = state.skillCovFunc), true)
      }

      val diffFunction = SkillsDiffFunction(scores, currSkillParams.skillMeanFunc,
        Some(gradientMask), diffFuncProgressListener, currSkillParams.skillCovFunc)

      val optimizer = new LBFGS[DenseVector[Double]](maxIter = 1, m = 6, tolerance = 1.0E-9)

      val optIters = optimizer.iterations(diffFunction, DenseVector(currSkillParams.skillCovFunc.getParams().toArray :+ logPerfStdDev)).toList
      val newParams = optIters.last.x

      val newSkillCovFunc = currSkillParams.skillCovFunc.withParams(newParams.data.dropRight(1))
      currSkillParams = SkillsModelParams(diffFunction.currSkillPriorMeanFunc, newSkillCovFunc)
      progressListener(currSkillParams, false)

      logger.info("======================================================")
      logger.info("Learning skills given opponent")
      logger.info("======================================================")

      val inferPlayerSkill = InferSkillGivenPlayer(currSkillParams, logPerfStdDev, scores)
      def getPlayerSkill(player: Player): PlayerSkill = inferPlayerSkill.inferSkill(player)

      val newSkillCovFunc2 = currSkillParams.skillCovFunc.withPlayerSkills(getPlayerSkill)
      currSkillParams = SkillsModelParams(currSkillParams.skillMeanFunc, newSkillCovFunc2)
    }

    currSkillParams

  }
}