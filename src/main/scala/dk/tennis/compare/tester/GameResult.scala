package dk.tennis.compare.tester

/**
 * @param points Single point is true if player1 wins, otherwise it's false.
 */
abstract class GameResult(
  val eventName: Option[String] = None,
  val player1: String,
  val player2: String,
  val player1Win: Option[Boolean] = None,
  val trueWinProb: Option[Double]= None,
  val timestamp: Option[Long] = None) {
  def containsPlayer(playerName: String): Boolean = player1.equals(playerName) || player2.equals(playerName)
}