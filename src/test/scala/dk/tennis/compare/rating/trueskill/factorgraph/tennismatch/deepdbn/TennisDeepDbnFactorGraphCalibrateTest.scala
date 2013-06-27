package dk.tennis.compare.rating.trueskill.factorgraph.tennismatch.deepdbn

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.infer.ep.GenericEP
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate

class TennisDeepDbnFactorGraphCalibrateTest {

  val logger = Logger(LoggerFactory.getLogger(getClass()))

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches).take(300)

  val performanceVariance = pow(25d / 16, 2)
  val skillTransVariance = pow(25d / 150, 2)

  @Test def calibrate {

    val tennisFactorGraph = TennisDeepDbnFactorGraph(skillTransVariance, performanceVariance)
    val results = gameResults.map(r => Result(r.player1, r.player2, r.player1Win.get))
    results.foreach(r => tennisFactorGraph.addResult(r))

    val ep = GenericEP(tennisFactorGraph.getFactorGraph())
    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)

    val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())
    val epSummary = epCalibrate.calibrate(100, progress)
    logger.debug("EP Summary: " + epSummary)

    assertEquals(13, epSummary.iterNum)

  }
}