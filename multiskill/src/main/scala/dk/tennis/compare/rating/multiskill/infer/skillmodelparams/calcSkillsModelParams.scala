package dk.tennis.compare.rating.multiskill.infer.skillmodelparams

import breeze.optimize.LBFGS
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer

object calcSkillsModelParams extends LazyLogging {

  def apply(priorSkillParams: SkillsModelParams, scores: Array[Score], gradientMask: Array[Double],
    progressListener: (SkillDiffFuncState) => Unit = (state) => {}, iterNum: Int, logPerfStdDev: Double): SkillsModelParams = {

    val diffFunction = SkillsDiffFunction(scores, priorSkillParams.skillMeanFunc,
      Some(gradientMask), progressListener, priorSkillParams.skillCovFunc)

    val optimizer = new LBFGS[DenseVector[Double]](maxIter = iterNum, m = 6, tolerance = 1.0E-9)

    val optIters = optimizer.iterations(diffFunction, DenseVector(priorSkillParams.skillCovFunc.getParams().toArray :+ logPerfStdDev)).toList
    val newParams = optIters.last.x

    val newSkillCovFunc = priorSkillParams.skillCovFunc.withParams(newParams.data.dropRight(1))

    val newSkillModelParams = SkillsModelParams(diffFunction.currSkillMeanFunc, newSkillCovFunc)
    newSkillModelParams

  }
}