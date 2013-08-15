package dk.tennis.compare.rating.multiskill.testutil

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import dk.tennis.compare.rating.multiskill.domain.PointResult
import org.junit._
import Assert._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

object MultiSkillTestUtil {

  def loadTennisMatches(yearFrom: Int, yearTo: Int): Seq[MatchResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    val matchResults = fromMatches(filteredMatches, new Random(0))

    matchResults
  }

  def fromMatches(matches: Seq[MatchComposite], random: Random = new Random(System.currentTimeMillis())): Seq[MatchResult] = {
    val gameResults = matches.map { m =>

      val player1 = m.matchFacts.playerAFacts.playerName
      val player2 = m.matchFacts.playerBFacts.playerName

      var points = List.fill(m.matchFacts.playerAFacts.totalServicePointsWon)(PointResult(player1, true)) :::
        List.fill(m.matchFacts.playerAFacts.totalServicePoints - m.matchFacts.playerAFacts.totalServicePointsWon)(PointResult(player1, false)) :::
        List.fill(m.matchFacts.playerBFacts.totalServicePointsWon)(PointResult(player2, true)) :::
        List.fill(m.matchFacts.playerBFacts.totalServicePoints - m.matchFacts.playerBFacts.totalServicePointsWon)(PointResult(player2, false))

      points = random.shuffle(points)

      while (!points.last.playerOnServe.equals(m.matchFacts.winner) || !points.last.playerOnServeWin) points = Random.shuffle(points)

      new MatchResult(
        m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        points,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName),
        numOfSets = m.tournament.numOfSet)
    }

    gameResults
  }

  def toTournaments(matches: Seq[MatchResult]): Seq[TournamentResult] = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  def assertPlayerSkill(expected: PlayerSkill, actual: PlayerSkill, delta: Double) = {
    assertEquals(expected.mean, actual.mean, delta)
    assertEquals(expected.variance, actual.variance, delta)
  }

  def assertPlayerSkills(expected: PlayerSkills, actual: PlayerSkills, delta: Double) = {
    assertEquals(expected.player, actual.player)
    assertEquals(expected.skillOnServe.mean, actual.skillOnServe.mean, delta)
    assertEquals(expected.skillOnServe.variance, actual.skillOnServe.variance, delta)

    assertEquals(expected.skillOnReturn.mean, actual.skillOnReturn.mean, delta)
    assertEquals(expected.skillOnReturn.variance, actual.skillOnReturn.variance, delta)
  }

  def assertMultiSkillParams(expected: MultiSkillParams, actual: MultiSkillParams, delta: Double) = {
    assertEquals(expected.skillOnServeTransVariance, actual.skillOnServeTransVariance, delta)
    assertEquals(expected.skillOnReturnTransVariance, actual.skillOnReturnTransVariance, delta)
    assertEquals(expected.perfVarianceOnServe, actual.perfVarianceOnServe, delta)
    assertEquals(expected.perfVarianceOnReturn, actual.perfVarianceOnReturn, delta)
  }
}