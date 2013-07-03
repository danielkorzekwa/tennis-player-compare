package dk.tennis.compare.rating.trueskill.factorgraph.point

import scala.math.pow
import scala.util.Random
import org.junit.Assert.assertEquals
import org.junit.Test
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.bayes.infer.ep.GenericEP
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.infer.ep.calibrate.fb.EPSummary
import org.apache.commons.lang.time.StopWatch

class TennisPointDbnFactorGraphTest {

  val logger = Logger(LoggerFactory.getLogger(getClass()))

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches, new Random(0)).take(50)

  val performanceVariance = pow(250d / 16, 2)
  val skillTransVariance = pow(25d / 150, 2)

  @Test def calibrate {

    val tennisFactorGraph = TennisPointDbnFactorGraph(skillTransVariance, performanceVariance)

    gameResults.foreach { r =>
      val pointResults = toPointResults(r)
      tennisFactorGraph.addPointResults(pointResults)
    }

    val ep = GenericEP(tennisFactorGraph.getFactorGraph(), threshold = 0.001)

    val timer = new StopWatch()
    timer.start()

    val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph(), threshold = 0.001)
    val iterTotal = epCalibrate.calibrate(10000, progress)
    logger.debug("Iter total: " + iterTotal)
    logger.debug("Time: " + timer.getTime())
    //  assertEquals(EPSummary(8, 87936), iterTotal)

  }

  private def toPointResults(gameResult: TennisResult): Seq[Result] = {
    val pointResults = gameResult.points.get.map { point =>

      if (point.playerOnServe.equals(gameResult.player1))
        Result(gameResult.player1, gameResult.player2, point.won)
      else if (point.playerOnServe.equals(gameResult.player2))
        Result(gameResult.player2, gameResult.player1, point.won)
      else throw new IllegalArgumentException("Player on serve not found")

    }
    pointResults
  }

  private def progress(currIter: Int) = println("EP iteration: " + currIter)
}