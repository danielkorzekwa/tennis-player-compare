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

  private val (priorSkillOnServe, priorSkillOnReturn) = (5d, 0)
  private val initialParams = DenseVector(-1.0394676060535801, 3.8382339487840085, 0.0032389722419957287, 8.282433925904247,log(0.00001),log(1), 2.3)
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