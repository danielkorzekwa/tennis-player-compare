package dk.tennis.compare.rating.multiskill.infer.skillmodelparams

import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import scala.Array.canBuildFrom

/**
 * @param priorSkillsGivenOpponent key - opponent name, value - player skills against opponent
 */
case class SkillsDiffFunction(scores: Array[Score], skillMeanFunc: (Player) => Double,
  gradientMask: Option[Array[Double]] = None, progressListener: (SkillDiffFuncState) => Unit = (state) => {},
  skillCovFunc: CovFunc) extends DiffFunction[DenseVector[Double]] with Logging {

  var currSkillPriorMeanFunc = skillMeanFunc
  var currSkillCovFunc = skillCovFunc

  var i = 0
  def calculate(params: DenseVector[Double]): (Double, DenseVector[Double]) = {

    if (gradientMask.isDefined)
      require(params.size == gradientMask.get.size, "Params and gradient mask size don't match")

    logger.info("params: %s".format(params.toString))

    val covarianceParams = params.data.dropRight(1)
    val logPerfStdDev = params.data.last

    val skillsCov = currSkillCovFunc.withParams(covarianceParams)
    val gp = GenericPerfDiffModel(currSkillPriorMeanFunc, skillsCov, logPerfStdDev, scores)
    try {
      gp.calibrateModel()
    } catch {
      case e: Exception => logger.warn("Calibrabion error")
    }

    val (loglik, df) = try {

      val (perfDiffs, perfDiffsMeanD, perfDiffsVarD) =
        gp.inferPerfDiffsWithD()

      val f = -OutcomeLik.totalLoglik(perfDiffs.map(p => p.perfDiff), scores, score => { score.player2.playerName.equals("Novak Djokovic"); true })

      val df = (0 until perfDiffsMeanD.numCols).map { i =>
        val meanD = perfDiffsMeanD.column(i)
        val varD = perfDiffsVarD.column(i)

        val partialDf = OutcomeLik.totalLoglikD(perfDiffs.map(p => p.perfDiff), meanD.toArray, varD.toArray, scores)

        partialDf

      }.toArray

      val dfWithMask = gradientMask match {
        case Some(gradientMask) => df.zip(gradientMask).map { case (df, mask) => df * mask }
        case None => df
      }

      //learning of the skills mean function
      val playerSkillMarginals: Array[Double] = gp.skillsFactorGraph.getPlayerSkillsMarginalMean().toArray
      val newPriorSkillMeanFunc = learnSkillMeanFunction(Score.toPlayers(scores), playerSkillMarginals)

     

      currSkillPriorMeanFunc = newPriorSkillMeanFunc
      currSkillCovFunc = skillsCov
      
       val state = SkillDiffFuncState(currSkillCovFunc)
      
      progressListener(state)

      (f, DenseVector(dfWithMask) * (-1d))

    } catch {
      case e: Exception => {
        logger.warn("Perf diff inference error", e)
        (Double.NaN, params.map(v => Double.NaN))
      }
    }

    logger.info("loglik: %.2f, d: %s,".format(loglik, df.toString))
    (loglik, df)
  }

  private def learnSkillMeanFunction(players: Seq[Player], playerSkillMarginals: Array[Double]): (Player) => Double = {

    val marginalsOnServe = players.zip(playerSkillMarginals).filter(p => p._1.onServe).map(_._2)
    val marginalsOnReturn = players.zip(playerSkillMarginals).filter(p => !p._1.onServe).map(_._2)

    val meanOnServe = marginalsOnServe.sum / marginalsOnServe.size
    val meanOnReturn = marginalsOnReturn.sum / marginalsOnReturn.size

    logger.info("New mean on serve/return: %.2f/%.2f".format(meanOnServe, meanOnReturn))
    def playerSkillMeanPrior(skillPriorMeanOnServe: Double, skillPriorMeanOnReturn: Double)(player: Player): Double = {
      if (player.onServe) skillPriorMeanOnServe else skillPriorMeanOnReturn
    }

    playerSkillMeanPrior(meanOnServe - meanOnReturn, 0)
  }

}
