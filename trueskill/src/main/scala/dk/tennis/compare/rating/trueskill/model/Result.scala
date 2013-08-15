package dk.tennis.compare.rating.trueskill.model

case class Result(player1: String, player2: String, player1Win: Boolean, player1PerfVar: Option[Double] = None,
  player2PerfVar: Option[Double] = None)