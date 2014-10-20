package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiffModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc

case class PastDataMatchModel(matchResults: IndexedSeq[MatchResult]) extends MatchModel {

  private val (priorSkillOnServe, priorSkillOnReturn) = (5d, 0)
  private val initialParams = DenseVector(-1.0394676060535801, 3.8382339487840085, 0.0032389722419957287, 8.282433925904247, 2.3)
  private val covarianceParams = initialParams.data.dropRight(1)
  private val logPerfStdDev = initialParams.data.last

  def predict(matchResult: MatchResult): MatchPrediction = {

    val predictionMatchIndex = matchResults.indexOf(matchResult)
    val pastMatchResults = if(predictionMatchIndex>=0) matchResults.take(predictionMatchIndex) else matchResults
    val evidenceScores = Score.toScores(pastMatchResults)

    val predictionScores = Score.toScores(matchResult).map(s => s.copy(pointsWon = None))

    val allScores = evidenceScores ++ predictionScores

    val allPredictions = calcMatchPredictions(allScores,matchResults :+ matchResult)
    val matchPrediction = allPredictions.last

    matchPrediction
  }

  private def calcMatchPredictions(scores: Array[Score],matchResults:IndexedSeq[MatchResult]): Seq[MatchPrediction] = {

    val infer = GenericPerfDiffModel(playerSkillMeanPrior, SkillOverTimeCovFunc(covarianceParams), logPerfStdDev, scores)
    infer.calibrateModel()

    val perfDiffs = infer.inferPerfDiffs()

    val matchPredictions = MatchPrediction.toMatchPredictions(scores, perfDiffs,matchResults)

    matchPredictions
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}