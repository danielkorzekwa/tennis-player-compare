package dk.tennis.compare.rating.multiskill.model.tournament

trait TournamentModel {

  /**
   *
   * @param draw First round pairs
   * @param winProb(player1,player2) Probability of winning a tennis match by player 1 against player 2
   *
   * @returns Map[player,winning probabilities]
   */
  def winningProbs(draw: Seq[Tuple2[String, String]], winProb: (String, String) => Double): Map[String, Double]
}