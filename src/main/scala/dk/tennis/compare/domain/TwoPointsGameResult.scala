package dk.tennis.compare.domain

class TwoPointsGameResult(
  eventName: Option[String] = None,
  player1: String,
  player2: String,
  player1Win: Option[Boolean] = None,
  trueWinProb: Option[Double] = None,
  timestamp: Option[Long] = None,
  points: Seq[Boolean]) extends GameResult(
  eventName,
  player1,
  player2,
  player1Win,
  trueWinProb,
  timestamp)