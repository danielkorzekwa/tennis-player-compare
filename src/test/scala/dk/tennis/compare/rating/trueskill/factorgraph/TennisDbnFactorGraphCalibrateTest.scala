package dk.tennis.compare.rating.trueskill.factorgraph

import org.junit._
import org.junit.Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import org.apache.commons.lang.time.StopWatch
import dk.tennis.compare.rating.trueskill.factorgraph.TennisDeepDbnFactorGraph

class TennisDbnFactorGraphCalibrateTest {

  val logger = Logger(LoggerFactory.getLogger(getClass()))

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches)

  val performanceVariance = pow(25d / 16, 2)
  val skillTransVariance = pow(25d / 150, 2)

  @Test def calibrate {

    val tennisFactorGraph = TennisDeepDbnFactorGraph(skillTransVariance, performanceVariance)
    //  val tennisFactorGraph = TennisDbnFactorGraph(skillTransVariance, performanceVariance)

    val results = gameResults.map(r => Result(r.player1, r.player2, r.player1Win.get))
    results.foreach(r => tennisFactorGraph.addResult(r))

    val timer = new StopWatch()
    timer.start()

    val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())
    val epSummary = epCalibrate.calibrate(100, progress)
    logger.debug("EP Summary: " + epSummary)

    logger.debug("Time: " + timer.getTime())
  }

  private def progress(currIter: Int) = println("EP iteration: " + currIter)
}