package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.GenericSkillCovFunc
import scala.math._

/**
 *  Leave one out match prediction model
 *
 */
case class LooMatchModel(matchResults: IndexedSeq[MatchResult]) extends MatchModel with Logging {

  private val (priorSkillOnServe, priorSkillOnReturn) = (4.96d, 0)
  private val initialParams = DenseVector(-0.8905121105461389, 3.430519279367633, 0.05980742993570404, 8.036933411241728, log(1e-10),  log(1), 2.3)
 //  private val initialParams = DenseVector(-0.8905121105461389, 3.430519279367633, 0.05980742993570404, 8.036933411241728, -1.7196515647057071, -0.0999651876799888, 2.3)
  private val covarianceParams = initialParams.data.dropRight(1)
  private val logPerfStdDev = initialParams.data.last

  logger.info("Computing match predictions...")
  private val matchPredictions = calcMatchPredictions()
  logger.info("Done")

  def predict(matchResult: MatchResult): MatchPrediction = {

    val matchPrediction = matchPredictions(matchResults.indexOf(matchResult))
    matchPrediction
  }

  private def calcMatchPredictions(): Seq[MatchPrediction] = {
    val scores = Score.toScores(matchResults)

    def createPlayersSkillsFactor(players: Array[Player]): SkillsFactor = MultiGPSkillsFactor3(playerSkillMeanPrior, GenericSkillCovFunc(covarianceParams), players)
    val infer = GenericPerfDiffModel(createPlayersSkillsFactor, logPerfStdDev, scores)
    infer.calibrateModel()

    val perfDiffs = infer.inferPerfDiffs()

    val matchPredictions = MatchPrediction.toMatchPredictions(scores, perfDiffs,matchResults)

    matchPredictions
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}