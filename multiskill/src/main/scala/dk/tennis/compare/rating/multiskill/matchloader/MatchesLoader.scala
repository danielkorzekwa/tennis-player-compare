package dk.tennis.compare.rating.multiskill.matchloader

import scala.util.Random
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.atp.api.tournament.TournamentAtpApi.Tournament
import scala.collection.immutable.HashSet
import dk.atp.api.domain.SurfaceEnum
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface

object MatchesLoader {

  def loadMatches(atpFile: String, yearFrom: Int, yearTo: Int, playersFilter: Set[String] = HashSet()): Seq[MatchResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(atpFile)

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.servicePointsTotal > 10 && m.matchFacts.playerBFacts.servicePointsTotal > 10)

    val matchResults = fromMatches(filteredMatches)
      .filter(r => playersFilter.isEmpty || (playersFilter.contains(r.player1) && playersFilter.contains(r.player2)))

    matchResults
  }

  private def fromMatches(matches: Seq[MatchComposite]): Seq[MatchResult] = {
    val gameResults = matches.map { m =>

      val surface = m.tournament.surface match {
        case SurfaceEnum.HARD => Surface.HARD
           case SurfaceEnum.CLAY => Surface.CLAY
      }
      
      val player1 = m.matchFacts.playerAFacts.playerName
      val player2 = m.matchFacts.playerBFacts.playerName

      val p1Stats = PlayerStats(
        m.matchFacts.playerAFacts.aces,
        m.matchFacts.playerAFacts.servicePointsWon, m.matchFacts.playerAFacts.servicePointsTotal)

      val p2Stats = PlayerStats(
        m.matchFacts.playerBFacts.aces,
        m.matchFacts.playerBFacts.servicePointsWon, m.matchFacts.playerBFacts.servicePointsTotal)

        
      new MatchResult(
        m.tournament.tournamentTime, m.tournament.tournamentName,surface,
        m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        m.tournament.tournamentTime,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName),
        numOfSets = m.tournament.numOfSet,
        p1Stats,
        p2Stats)
    }

    gameResults
  }

}