package dk.tennis.compare.rating.multiskill.infer.matchprob.givenpastmatchresults

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.infer.matchprob.MatchPrediction
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults

case class InferMatchProbGivenPastMatchResults(matchResults: IndexedSeq[MatchResult]) {

  def predict(matchResult: MatchResult): MatchPrediction = {

    val predictionMatchIndex = matchResults.indexOf(matchResult)
    val pastMatchResults = if (predictionMatchIndex >= 0) matchResults.take(predictionMatchIndex) else matchResults

    val matchPrediction = InferMatchProbGivenMatchResults(pastMatchResults).predict(matchResult)
    matchPrediction
  }
}