package dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresultsloo

import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.infer.matchprob.MatchPrediction
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

case class InferMatchProbGivenMatchResultsLoo(matchResults: IndexedSeq[MatchResult]) extends Logging {

  private val skillCovParams = Array(
    2.022132402833459, // opponent 
    2.335108140472381, 0.8585245019924235, 0.6068550430203135, // surface
    -0.7964151980207621, 3.0080055487894057, 0.422376471909165, 7.932430851981252 //time
    )

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

    val skillCovFunc = SkillCovFunc(skillCovParams)

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