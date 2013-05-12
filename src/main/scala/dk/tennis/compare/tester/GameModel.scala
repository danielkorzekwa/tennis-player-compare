package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.domain.GameResult

trait GameModel {

  /**
   * Returns the probability of winning the game by player A.
   */
  def gameProb(r: GameResult):Option[Double]
  
  def addGameResult(r:GameResult)
}