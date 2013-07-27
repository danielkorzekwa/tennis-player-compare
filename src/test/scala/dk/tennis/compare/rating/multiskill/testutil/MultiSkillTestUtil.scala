package dk.tennis.compare.rating.multiskill.testutil

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.util.Random
import dk.tennis.compare.rating.multiskill.domain.PointResult

object MultiSkillTestUtil {

  def loadTennisMatches(yearFrom: Int, yearTo: Int): Seq[MatchResult] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

    val matches = (yearFrom to yearTo).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    val gameResults = TennisResult.fromMatches(filteredMatches, new Random(0))

    val matchResults = toMatchResults(gameResults)

    matchResults
  }

  private def toMatchResults(gameResults: Seq[TennisResult]): Seq[MatchResult] = {
    val matchResults = gameResults.map { r =>

      val pointResults = r.points.get.map { point =>

        if (point.playerOnServe.equals(r.player1)) PointResult(r.player1, point.won)
        else if (point.playerOnServe.equals(r.player2)) PointResult(r.player2, point.won)
        else throw new IllegalArgumentException("Player on serve not found")
      }

      val matchResult = MatchResult(r.player1, r.player2, pointResults,r.player1Win.get,r.numOfSets)
      matchResult
    }

    matchResults

  }
}