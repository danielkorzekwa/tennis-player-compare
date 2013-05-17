package dk.tennis.compare.game.tennis.domain

import dk.tennis.compare.tester.GameResult

/**
 * @param points Point is true if player 1 wins, otherwise it is false.
 */
case class TennisResult(

  override val eventName: Option[String] = None,
  override val player1: String,
  override val player2: String,
  override val player1Win: Option[Boolean] = None,
  override val trueWinProb: Option[Double] = None,
  override val timestamp: Option[Long] = None,

  val numOfSets: Int,
  val player1ServicePointsWonPct: Option[Double] = None,
  val player2ServicePointsWonPct: Option[Double] = None,
  val points:Option[Seq[Boolean]])

  extends GameResult(
    eventName,
    player1,
    player2,
    player1Win,
    trueWinProb,
    timestamp) {

}