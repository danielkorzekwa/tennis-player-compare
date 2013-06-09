package dk.tennis.compare.rating.trueskill.learn

import org.junit._
import org.junit.Assert._
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult

class MatchTrueSkillEMLearnTest {

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2010 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches)

  val performanceVariance = pow(25d / 16, 2)

  @Test def test_empty_results {

    val skillTransVariance = pow(25d / 150, 2)

    val results = Nil

    val learnedVariance = MatchTrueSkillEMLearn.learn(skillTransVariance, performanceVariance, results, maxIter = 5)
    assertEquals(Double.NaN, learnedVariance, 0.0001)
  }

  @Test def test_three {

    val skillTransVariance = pow(25d / 150, 2)

    val results = List(Result("p1", "p2", true), Result("p1", "p2", true), Result("p1", "p3", false), Result("p1", "p3", false), Result("p2", "p3", true))

    val learnedVariance = MatchTrueSkillEMLearn.learn(skillTransVariance, performanceVariance, results, maxIter = 10)
    assertEquals(0.0267, learnedVariance, 0.0001)
  }

  @Test def test_atp_results_2010_to_2011 {

    val skillTransVariance = 0.25
    val results = gameResults.map(r => Result(r.player1, r.player2, r.player1Win.get))

    val learnedVariance = MatchTrueSkillEMLearn.learn(skillTransVariance, performanceVariance, results, maxIter = 3)
    assertEquals(0.2367, learnedVariance, 0.0001)
  }

}