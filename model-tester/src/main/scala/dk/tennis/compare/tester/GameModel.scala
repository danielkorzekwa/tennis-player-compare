package dk.tennis.compare.tester

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

trait GameModel {

  /**
   * Returns the probability of winning the game by player A.
   */
  def gameProb(tournament: TournamentResult, matchResult: MatchResult):Option[Double]
  
  def addGameResult(tournament:TournamentResult,tennisMatch:MatchResult)
}