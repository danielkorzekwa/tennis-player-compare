package dk.tennis.compare.rating.multiskill.matchloader

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface

object generateMatches {


  def apply(players: Seq[String], rounds: Int): Seq[MatchResult] = {

    val matchTime = new AtomicInteger()

    def genMatches(): Seq[MatchResult] = {
      val matches = players.flatMap { p =>

        val opponents = players//.filter(opponent => !opponent.equals(p))
        opponents.map(opponent => toMatchResult(p, opponent, new Date(matchTime.getAndIncrement())))
      }

      matches
    }

    List.fill(rounds)(genMatches()).flatten
  }

  private def toMatchResult(player1: String, player2: String, matchTime: Date): MatchResult = {

    val p1Stats = PlayerStats(0, 100, 100)
    val p2Stats = PlayerStats(0, 100, 100)
    MatchResult(matchTime, "tournament name",Surface.HARD, player1, player2, matchTime, true, 2, p1Stats, p2Stats)
  }
}