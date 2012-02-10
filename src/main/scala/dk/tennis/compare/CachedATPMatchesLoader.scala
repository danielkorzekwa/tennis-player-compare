package dk.tennis.compare

import dk.atp.api.TournamentAtpApi
import scala.collection.mutable.Map
import ATPMatchesLoader._

class CachedATPMatchesLoader(tournamentApi: TournamentAtpApi) extends ATPMatchesLoader {

  /**key - year.*/
  private val cachedMatches: Map[Int, List[MatchComposite]] = Map()

  def loadMarkets(year: Int): List[MatchComposite] = cachedMatches.getOrElseUpdate(year, loadMarketsFromATP(year))

  private def loadMarketsFromATP(year: Int): List[MatchComposite] = {

    collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(16)

    val tournaments = tournamentApi.parseTournaments(year).filter(!_.tournamentUrl.isEmpty())

    val matchesComposite = tournaments.par.flatMap { tournament =>

      val tennisMatches = tournamentApi.parseTournament(tournament.tournamentUrl)

      tennisMatches.par.map { tennisMatch =>
        val matchFacts = tournamentApi.parseMatchFacts(tennisMatch.matchFactsUrl)

        MatchComposite(tournament, tennisMatch, matchFacts)
      }
    }

    matchesComposite.toList
  }
}