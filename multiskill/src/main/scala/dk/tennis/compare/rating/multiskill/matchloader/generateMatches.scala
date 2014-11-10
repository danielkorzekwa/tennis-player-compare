package dk.tennis.compare.rating.multiskill.matchloader

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import scala.util.Random
import Surface._

object generateMatches {

  def apply(players: Seq[String], rounds: Int, randSeed: Int): Seq[MatchResult] = {

    val random = new Random(randSeed)
    val matchTime = new AtomicInteger()

    def genMatches(): Seq[MatchResult] = {
      val matches = players.flatMap { p =>

        val opponents = players //.filter(opponent => !opponent.equals(p))
        opponents.map(opponent => toMatchResult(p, opponent, new Date(matchTime.getAndIncrement() * 1000L), random))
      }

      matches
    }

    List.fill(rounds)(genMatches()).flatten
  }

  private def toMatchResult(player1: String, player2: String, matchTime: Date, random: Random): MatchResult = {

    val p1Stats = PlayerStats(0, 100, 100)
    val p2Stats = PlayerStats(0, 100, 100)

    val surface = Surface(random.nextInt(3))
    MatchResult(matchTime, "tournament name", surface, player1, player2, matchTime, true, 2, p1Stats, p2Stats)
  }
}