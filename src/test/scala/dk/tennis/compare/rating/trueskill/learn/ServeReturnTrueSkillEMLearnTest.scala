package dk.tennis.compare.rating.trueskill.learn

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennis.compare.rating.trueskill.model.Result
import scala.math._

class ServeReturnTrueSkillEMLearnTest {

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2010 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches).take(100)

  val pointResults = toPointResults(gameResults)

  val performanceVariance = pow(250d / 16, 2)
  @Test def test_atp_results_2010_to_2011 {

    //  val skillTransVariance = pow(25d / 3000, 2)
    val skillTransVariance = 0.25

    val learnedVariance = ServeReturnTrueSkillEMLearn.learn(skillTransVariance, performanceVariance, pointResults, maxIter = 10)
    assertEquals(0.25011, learnedVariance, 0.001)
  }

  private def toPointResults(gameResults: Seq[TennisResult]): Seq[Result] = {
    val pointResults = gameResults.flatMap { r =>

      r.points.get.takeRight(10).map { point =>

        if (point.playerOnServe.equals(r.player1))
          Result(r.player1, r.player2, point.won)
        else if (point.playerOnServe.equals(r.player2))
          Result(r.player2, r.player1, point.won)
        else throw new IllegalArgumentException("Player on serve not found")
      }
    }
    pointResults
  }
}