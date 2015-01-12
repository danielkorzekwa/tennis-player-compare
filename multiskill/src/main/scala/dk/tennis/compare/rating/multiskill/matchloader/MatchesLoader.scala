package dk.tennis.compare.rating.multiskill.matchloader

import scala.util.Random
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import scala.collection.immutable.HashSet
import dk.atp.api.domain.SurfaceEnum
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.atp.api.domain.TennisMatch

object MatchesLoader {

  def loadMatches(atpFile: String, yearFrom: Int, yearTo: Int, playersFilter: Set[String] = HashSet()): Seq[MatchResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(atpFile)

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.p1Facts.servicePointsTotal > 10 && m.p2Facts.servicePointsTotal > 10)

    val matchResults = fromMatches(filteredMatches)
      .filter(r => playersFilter.isEmpty || (playersFilter.contains(r.player1) && playersFilter.contains(r.player2)))

    matchResults
  }

  private def fromMatches(matches: Seq[TennisMatch]): Seq[MatchResult] = {
    val gameResults = matches.map { m =>

      val surface = m.tournament.surface match {
        case SurfaceEnum.HARD => Surface.HARD
        case SurfaceEnum.CLAY => Surface.CLAY
        case SurfaceEnum.GRASS => Surface.GRASS
      }

      val player1 = m.player1
      val player2 = m.player2

      val p1Stats = PlayerStats(
        m.p1Facts.aces,
        m.p1Facts.servicePointsWon, m.p1Facts.servicePointsTotal)

      val p2Stats = PlayerStats(
        m.p2Facts.aces,
        m.p2Facts.servicePointsWon, m.p2Facts.servicePointsTotal)

      new MatchResult(
        m.tournament.tournamentTime, m.tournament.tournamentName, surface,
        m.player1,
        m.player2,
        m.tournament.tournamentTime,
        m.winner.equals(m.player1),
        numOfSets = m.tournament.numOfSet,
        p1Stats,
        p2Stats)
    }

    gameResults
  }

}