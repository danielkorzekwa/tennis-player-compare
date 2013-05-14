package dk.tennis.compare.game.twopointsgame

import dk.tennis.compare.tester.GameResult

case class TwoPointsGameResult(
  override val eventName: Option[String] = None,
  override val player1: String,
  override val player2: String,
  override val player1Win: Option[Boolean] = None,
  override val trueWinProb: Option[Double] = None,
  override val timestamp: Option[Long] = None,
  player1ExpectedPointProb: Double,
  points: Seq[Boolean])

  extends GameResult(
    eventName,
    player1,
    player2,
    player1Win,
    trueWinProb,
    timestamp)