package dk.tennis.compare.rating.multiskill.factorgraph

import scala.math.pow
import scala.util.Random
import org.junit.Test
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import org.apache.commons.lang.time.StopWatch
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader

class TennisDbnFactorGraphTest {

  val logger = Logger(LoggerFactory.getLogger(getClass()))

  val atpFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tournaments = MatchesLoader.loadTournaments(atpFile,2010, 2011)

  val multiSkillParams = MultiSkillParams(
    skillOnServeTransVariance = 0.02,
    skillOnReturnTransVariance = 0.02,
    priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
    perfVarianceOnServe = 200, perfVarianceOnReturn = 100)

  @Test def calibrate {


    val tennisFactorGraph = TennisDbnFactorGraph(multiSkillParams)

  //  matchResults.foreach(r => tennisFactorGraph.addTennisMatch(r))
     tournaments.foreach(t => tennisFactorGraph.addTournament(t))

    println("Factors num: " + tennisFactorGraph.getFactorGraph.getFactorNodes.size)
    println("Variables num: " + tennisFactorGraph.getFactorGraph.getVariables.size)

    val timer = new StopWatch()
    timer.start()

    val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())
    val iterTotal = epCalibrate.calibrate(100, progress)
    logger.debug("Iter total: " + iterTotal)
    logger.debug("Time: " + timer.getTime())
    //  assertEquals(EPSummary(8, 87936), iterTotal)

    val ep = GenericEP(tennisFactorGraph.getFactorGraph())
    val varIdsOnServe = tennisFactorGraph.getSkillVarIdsOnServe()("Roger Federer")
    varIdsOnServe.takeRight(1000).foreach(vId => println("Roger Federer on serve:" + ep.marginal(vId)))

    println("----------------------------------------------")

    val varIdsOnReturn = tennisFactorGraph.getSkillVarIdsOnReturn()("Roger Federer")
    varIdsOnReturn.takeRight(1000).foreach(vId => println("Roger Federer on return:" + ep.marginal(vId)))

  }

  private def progress(currIter: Int) = println("EP iteration: " + currIter)
}