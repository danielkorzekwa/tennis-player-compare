package dk.tennis.compare
import dk.tennis.compare.domain.Market
import dk.atp.api.TournamentAtpApi._
import java.util.Date
import dk.atp.api.AtpWorldTourApi.SurfaceEnum._
import dk.atp.api.TournamentAtpApi
import TournamentAtpApi._
import org.joda.time.DateTime
import scala.collection.mutable.Map
import scala.collection.immutable.TreeMap
import scala.Math._
import CachedTournamentLookup._

object CachedTournamentLookup {
  case class MatchComposite(tournament: Tournament, tennisMatch: Match, matchFacts: MatchFacts)
}

class CachedTournamentLookup(tournamentAtpApi: TournamentAtpApi) extends TournamentLookup {

  /**key - year.*/
  val cachedMatches: Map[Int, List[MatchComposite]] = Map()

  /**Look for tournament matching given market.*/
  def lookup(market: Market): Option[Tournament] = {
    val year = new DateTime(market.scheduledOff).getYear()

    val matches = cachedMatches.getOrElseUpdate(year, loadMarkets(year))

    def playerNames(matchFacts: MatchFacts): List[String] = matchFacts.playerAFacts.playerName :: matchFacts.playerBFacts.playerName :: Nil

    val filteredMatches = matches.filter(m => playerNames(m.matchFacts).sorted.equals(market.runnerMap.values.toList.sorted))
    val timeDiffMatches = TreeMap[Long, Tournament]() ++ filteredMatches.map(m => (abs(market.scheduledOff.getTime() - m.tournament.tournamentTime.getTime()), m.tournament))

    if (timeDiffMatches.isEmpty) None else Option(timeDiffMatches.head._2)
  }

  private def loadMarkets(year: Int): List[MatchComposite] = {
    collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(16)

    val tournaments = tournamentAtpApi.parseTournaments(year).filter(!_.tournamentUrl.isEmpty())

    val matchesComposite = tournaments.par.flatMap { tournament =>

      val tennisMatches = tournamentAtpApi.parseTournament(tournament.tournamentUrl)

      tennisMatches.par.map { tennisMatch =>
        val matchFacts = tournamentAtpApi.parseMatchFacts(tennisMatch.matchFactsUrl)

        MatchComposite(tournament, tennisMatch, matchFacts)
      }
    }

    matchesComposite.toList
  }

}