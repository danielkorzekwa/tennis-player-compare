package dk.tennis.compare.rating.multiskill.matchloader

import scala.util.Random

import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.atp.api.tournament.TournamentAtpApi.Tournament

object MatchesLoader {

  def loadMatches(atpFile: String, yearFrom: Int, yearTo: Int): Seq[MatchResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile(atpFile)

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.servicePointsTotal > 10 && m.matchFacts.playerBFacts.servicePointsTotal > 10)

    val matchResults = fromMatches(filteredMatches)

    matchResults
  }


  private def fromMatches(matches: Seq[MatchComposite]): Seq[MatchResult] = {
    val gameResults = matches.map { m =>

      val player1 = m.matchFacts.playerAFacts.playerName
      val player2 = m.matchFacts.playerBFacts.playerName

      val p1Stats = PlayerStats(
        m.matchFacts.playerAFacts.aces,
        m.matchFacts.playerAFacts.servicePointsWon, m.matchFacts.playerAFacts.servicePointsTotal)

      val p2Stats = PlayerStats(
        m.matchFacts.playerBFacts.aces,
        m.matchFacts.playerBFacts.servicePointsWon, m.matchFacts.playerBFacts.servicePointsTotal)

      new MatchResult(
        m.tournament.tournamentTime, m.tournament.tournamentName,
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