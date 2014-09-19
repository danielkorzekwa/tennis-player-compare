package dk.tennis.compare.rating.multiskill.learn

import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.SkillCovFunc

case class SkillsDiffFunction(scores: Array[Score], skillPriorMeanOnServe: Double, skillPriorMeanOnReturn: Double, createPlayerCovFunc: (Array[Double]) => SkillCovFunc, gradientMask: Option[Array[Double]] = None, trueLoglik: Option[Double] = None) extends DiffFunction[DenseVector[Double]] with Logging {

  private var currSkillPriorMeanOnServe = skillPriorMeanOnServe
  private var currSkillPriorMeanOnReturn = skillPriorMeanOnReturn

  def calculate(params: DenseVector[Double]): (Double, DenseVector[Double]) = {

    if (gradientMask.isDefined)
      require(params.size == gradientMask.get.size, "Params and gradient mask size don't match")

    logger.info("params: %s, priorMean(serve/return): %.2f / %.2f".format(params.toString, currSkillPriorMeanOnServe, currSkillPriorMeanOnReturn))

    val covarianceParams = params.data.dropRight(1)
    val logPerfStdDev = params.data.last

    def createPlayersSkillsFactor(players: Array[Player]): SkillsFactor = MultiGPSkillsFactor3(playerSkillMeanPrior, createPlayerCovFunc(covarianceParams), players)

    val gp = GenericPerfDiffModel(createPlayersSkillsFactor, logPerfStdDev, scores)
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
      val (newPriorSkillMeanOnServe, newPriorSkillMeanOnReturn) = learnSkillMeanFunction(Score.toPlayers(scores), playerSkillMarginals)
      currSkillPriorMeanOnServe = newPriorSkillMeanOnServe
      currSkillPriorMeanOnReturn = newPriorSkillMeanOnReturn

      if (trueLoglik.isDefined) logger.info("Loglik actual-true delta: " + (f - trueLoglik.get))

      (f, DenseVector(dfWithMask) * (-1d))

    } catch {
      case e: Exception => {
        logger.warn("Perf diff inference error")
        (Double.NaN, params.map(v => Double.NaN))
      }
    }
    
    logger.info("loglik: %.2f, d: %s,".format(loglik, df.toString))
    (loglik, df)
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) currSkillPriorMeanOnServe else currSkillPriorMeanOnReturn
  }

  private def learnSkillMeanFunction(players: Seq[Player], playerSkillMarginals: Array[Double]): Tuple2[Double, Double] = {

    val marginalsOnServe = players.zip(playerSkillMarginals).filter(p => p._1.onServe).map(_._2)
    val marginalsOnReturn = players.zip(playerSkillMarginals).filter(p => !p._1.onServe).map(_._2)

    val meanOnServe = marginalsOnServe.sum / marginalsOnServe.size
    val meanOnReturn = marginalsOnReturn.sum / marginalsOnReturn.size
    (meanOnServe - meanOnReturn, 0)
  }
}
