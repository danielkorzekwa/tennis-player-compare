package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PointResult

class GenericMultiSkillEMLearnTest {

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches)

  val pointResults = toPointResults(gameResults)

  val performanceVariance = pow(25d / 16, 2)

  @Ignore @Test def test_empty_results {

    val skillTransVariance = pow(25d / 150, 2)

    val results = Nil

    val learnedVariance = GenericMultiSkillEMLearn.learn(skillTransVariance, performanceVariance, results, maxIter = 5)
    assertEquals(Double.NaN, learnedVariance, 0.0001)
  }

  @Ignore @Test def test_three {

    val skillTransVariance = pow(25d / 150, 2)

    val results = List(PointResult("p1", true), PointResult("p1", true), PointResult("p1", false), PointResult("p1", false), PointResult("p2", true))

    val learnedVariance = GenericMultiSkillEMLearn.learn(skillTransVariance, performanceVariance, results, maxIter = 10)
    assertEquals(0.0267, learnedVariance, 0.0001)
  }

  @Ignore @Test def test_atp_results_2010_to_2011 {

    val skillTransVariance = 0.25

    val learnedVariance = GenericMultiSkillEMLearn.learn(skillTransVariance, performanceVariance, pointResults, maxIter = 10)
  }

  private def toPointResults(gameResults: Seq[TennisResult]): Seq[PointResult] = {
    val pointResults = gameResults.flatMap { r =>

      r.points.get.takeRight(1).map { point =>

        if (point.playerOnServe.equals(r.player1))
          PointResult(r.player1, point.won)
        else if (point.playerOnServe.equals(r.player2))
          PointResult(r.player2, point.won)
        else throw new IllegalArgumentException("Player on serve not found")
      }
    }
    pointResults
  }
}