package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.MultiGPSkillsFactor3
import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong
import dk.tennis.compare.rating.multiskill.model.perfdiff.GenericPerfDiff

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

    val allPredictions = calcMatchPredictions(allScores)
    val matchPrediction = allPredictions.last

    matchPrediction
  }

  private def calcMatchPredictions(scores: Array[Score]): Seq[MatchPrediction] = {

    def createPlayersSkillsFactor(players: Array[Player]): SkillsFactor = MultiGPSkillsFactor3(playerSkillMeanPrior, PlayerCovFuncShortLong(covarianceParams), players)
    val infer = GenericPerfDiff(createPlayersSkillsFactor, logPerfStdDev, scores)
    infer.calibrateModel()

    val perfDiffs = infer.inferPerfDiffs()

    val matchPredictions = MatchPrediction.toMatchPredictions(scores, perfDiffs)

    matchPredictions
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}