package dk.tennis.compare.rating.trueskill.factorgraph.tennismatch

import org.junit._
import org.junit.Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.infer.ep.GenericEP
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.model.factorgraph.GenericFactorGraph
import scala.util.Random
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.infer.ep.calibrate.lrug.LRUGateEPCalibrate

class TennisDbnFactorGraphCalibrateTest {

  val logger = Logger(LoggerFactory.getLogger(getClass()))

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches).take(500)

  val performanceVariance = pow(25d / 16, 2)
  val skillTransVariance = pow(25d / 150, 2)

  @Test def calibrate {

    val tennisFactorGraph = TennisDbnFactorGraph(skillTransVariance, performanceVariance)
    val results = gameResults.map(r => Result(r.player1, r.player2, r.player1Win.get))
    results.foreach(r => tennisFactorGraph.addResult(r))
    println("Num of factor graphs: " + tennisFactorGraph.getFactorGraphs.size)

    val g = GenericFactorGraph()
    Random.shuffle(tennisFactorGraph.getFactorGraph().getFactorNodes).foreach(f => g.addFactor(f.factor))
    val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())
    val iterTotal = epCalibrate.calibrate(1000, progress)
    logger.debug("Iter total: " + iterTotal)

    //    val epCalibrate = LRUGateEPCalibrate(tennisFactorGraph.getFactorGraph())
    //    val msgNum = epCalibrate.calibrate
    //    logger.debug("Msg total: " + msgNum)

  }

  private def progress(currIter: Int) = {} //println("EP iteration: " + currIter)  
}