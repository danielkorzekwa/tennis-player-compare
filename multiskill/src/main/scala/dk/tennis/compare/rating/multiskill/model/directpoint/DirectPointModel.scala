package dk.tennis.compare.rating.multiskill.model.directpoint

import dk.tennis.compare.rating.multiskill.model.od.Game

trait DirectPointModel {

  def processGame(game: Game)

   /**
   * Returns the probability of winning a point for a future game. [player1ProbOnOffence,player2ProbOnOffence]
   */
  def pointProb(game:Game):Tuple2[Double, Double]

}