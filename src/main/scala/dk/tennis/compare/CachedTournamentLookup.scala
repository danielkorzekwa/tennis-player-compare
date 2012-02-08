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

class CachedTournamentLookup(tournamentAtpApi: TournamentAtpApi) extends TournamentLookup {

  /**key - year.*/
  val cachedTournaments: Map[Int, List[Tournament]] = Map()

  /**Look for tournament matching given market.*/
  def lookup(market: Market): Option[Tournament] = {
    val year = new DateTime(market.scheduledOff).getYear()

    val tournaments = cachedTournaments.getOrElseUpdate(year, tournamentAtpApi.parseTournaments(year))

    def matchMarket(tournament: Tournament): Boolean = {
      tournament.matches.exists(m => { m.players.sorted.equals(market.runnerMap.values.toList.sorted) })
    }

    val filteredTournaments = tournaments.filter(matchMarket(_))
    val timeDiffTournaments = TreeMap[Long, Tournament]() ++ filteredTournaments.map(t => (abs(market.scheduledOff.getTime() - t.tournamentTime.getTime()), t))

    if (timeDiffTournaments.isEmpty) None else Option(timeDiffTournaments.head._2)
  }

}