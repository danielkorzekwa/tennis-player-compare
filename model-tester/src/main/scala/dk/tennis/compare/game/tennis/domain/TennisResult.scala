package dk.tennis.compare.game.tennis.domain

import dk.tennis.compare.tester.GameResult
import java.util.Date
import dk.atp.api.domain.MatchComposite
import scala.util.Random

case class TennisResult(

  override val eventName: Option[String] = None,
  override val player1: String,
  override val player2: String,
  override val player1Win: Option[Boolean] = None,
  override val trueWinProb: Option[Double] = None,
  override val timestamp: Option[Date] = None,

  val numOfSets: Int,
  val player1ServicePointsWonPct: Option[Double] = None,
  val player2ServicePointsWonPct: Option[Double] = None,
  val points: Option[Seq[TennisPoint]],
  val round:Option[String])

  extends GameResult(
    eventName,
    player1,
    player2,
    player1Win,
    trueWinProb,
    timestamp) {

}

object TennisResult {

  def fromMatches(matches: Seq[MatchComposite], random:Random = new Random(System.currentTimeMillis())): Seq[TennisResult] = {
    val gameResults = matches.map { m =>

      val player1 = m.matchFacts.playerAFacts.playerName
      val player2 = m.matchFacts.playerBFacts.playerName

      var points = List.fill(m.matchFacts.playerAFacts.totalServicePointsWon)(TennisPoint(player1, true)) :::
        List.fill(m.matchFacts.playerAFacts.totalServicePoints - m.matchFacts.playerAFacts.totalServicePointsWon)(TennisPoint(player1, false)) :::
        List.fill(m.matchFacts.playerBFacts.totalServicePointsWon)(TennisPoint(player2, true)) :::
        List.fill(m.matchFacts.playerBFacts.totalServicePoints - m.matchFacts.playerBFacts.totalServicePointsWon)(TennisPoint(player2, false))

      points = random.shuffle(points)

      while (!points.last.playerOnServe.equals(m.matchFacts.winner) || !points.last.won) points = Random.shuffle(points)

      new TennisResult(
        eventName = Some(m.tournament.tournamentName),
        player1 = m.matchFacts.playerAFacts.playerName,
        player2 = m.matchFacts.playerBFacts.playerName,
        player1Win = Some(m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)),
        trueWinProb = None,
        timestamp = Some(new Date(m.tournament.tournamentTime.getTime())),
        numOfSets = m.tournament.numOfSet,
        player1ServicePointsWonPct = Some(m.matchFacts.playerAFacts.totalServicePointsWonPct),
        player2ServicePointsWonPct = Some(m.matchFacts.playerBFacts.totalServicePointsWonPct),
        points = Some(points),
        round =Some(m.matchFacts.round))
    }

    gameResults
  }
}