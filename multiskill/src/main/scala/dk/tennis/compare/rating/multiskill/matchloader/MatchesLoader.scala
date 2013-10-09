package dk.tennis.compare.rating.multiskill.matchloader

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerStats

object MatchesLoader {

  def loadTournaments(atpFile: String, yearFrom: Int, yearTo: Int): Seq[TournamentResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.servicePointsTotal > 10 && m.matchFacts.playerBFacts.servicePointsTotal > 10)

    val matchesByTournament: Map[Tournament, Seq[MatchComposite]] = filteredMatches.groupBy(m => m.tournament)

    val random = new Random(0)
    val tournaments = matchesByTournament.keys.toList.sortBy(t => t.tournamentTime).map { t =>
      val matches = matchesByTournament(t)
      val matchResults = fromMatches(matches, random)
      val players = matchResults.flatMap(r => List(r.player1, r.player2)).distinct

      TournamentResult(t.tournamentTime, t.tournamentName, players, matchResults)
    }

    tournaments
  }

  private def fromMatches(matches: Seq[MatchComposite], random: Random = new Random(System.currentTimeMillis())): Seq[MatchResult] = {
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
        m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName),
        numOfSets = m.tournament.numOfSet,
        p1Stats,
        p2Stats)
    }

    gameResults
  }

}