package dk.tennis.compare.rating.multiskill.model.matchmodel

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

trait MatchModel {

  def predict(matchResult:MatchResult):MatchPrediction
}