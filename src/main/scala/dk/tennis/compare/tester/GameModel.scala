package dk.tennis.compare.tester

trait GameModel {

  /**
   * Returns the probability of winning the game by player A.
   */
  def gameProb(r: GameResult):Option[Double]
  
  def addGameResult(r:GameResult)
}