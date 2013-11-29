package dk.tennis.compare.rating.multiskill.model.directpoint

import dk.tennis.compare.rating.multiskill.model.od.Game

trait DirectPointModel {

  def processGame(game: Game)

  /**
   * Returns the probability of winning a point. [thisPlayerProbOnOffence,opponentProbOnOffence]
   */
  def pointProb(opponent: String): Tuple2[Double, Double]

}