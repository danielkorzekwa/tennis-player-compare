package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc

/**
 *  Leave one out match prediction model
 *
 */
case class LooMatchModel(matchResults: IndexedSeq[MatchResult]) extends MatchModel with Logging {

  private val (priorSkillOnServe, priorSkillOnReturn) = (4.65d, 0)
  private val logPerfStdDev = 2.3

  logger.info("Computing match predictions...")
  private val matchPredictions = calcMatchPredictions()
  logger.info("Done")

  def predict(matchResult: MatchResult): MatchPrediction = {

    val matchPrediction = matchPredictions(matchResults.indexOf(matchResult))
    matchPrediction
  }

  private def calcMatchPredictions(): Seq[MatchPrediction] = {
    val scores = Score.toScores(matchResults)

    val skillCovParams = Array(
      0.9357633121804758, // opponent 
      0.12518641550453374, 0.0849608747727257, 0.051168632471147744, // surface
      -0.01613784259730807, 3.437620635696705, 0.3105890088146454, 5.927936432818671 //time
      )
    val skillCovFunc = SkillCovFunc(skillCovParams)

    // val skillCovFunc = OpponentOverTimeCovFunc.fromFile("target/skillCovFunc")
    val infer = GenericPerfDiffModel(playerSkillMeanPrior, skillCovFunc, logPerfStdDev, scores)
    infer.calibrateModel()

    val perfDiffs = infer.inferPerfDiffs()

    val matchPredictions = MatchPrediction.toMatchPredictions(scores, perfDiffs, matchResults)

    matchPredictions
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}