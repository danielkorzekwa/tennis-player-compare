package dk.tennis.compare.rating.multiskill.factorgraph.factor

import org.junit._
import Assert._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import scala.math._
import dk.atp.api.CSVATPMatchesLoader
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.util.Random
import dk.atp.api.domain.SurfaceEnum._
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor

class TennisMatchFactorTest {

  val p1SkillVarId = 10
  val p2SkillVarId = 20
  val outcomeVarId = 30

  val perfVariance = pow(25d / 6, 2)

  @Test def variable_marginal_player1_wins {
    val p1SkillFactor = new GaussianFactor(p1SkillVarId, 4, 81)
    val p2SkillFactor = new GaussianFactor(p2SkillVarId, 41, 25)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance, Vector(true))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)

    assertEquals(2, epCalibrate.calibrate(100, progress).iterNum)

    val outcomeMarginal = ep.marginal(outcomeVarId)
    assertEquals(1, outcomeMarginal.getValue((outcomeVarId, 0)), 0.00001)
    assertEquals(0, outcomeMarginal.getValue((outcomeVarId, 1)), 0.00001)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(27.1744, skill1Marginal.m, 0.0001)
    assertEquals(37.4973, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(33.8473, skill2Marginal.m, 0.0001)
    assertEquals(20.8559, skill2Marginal.v, 0.0001)
  }

  @Test def variable_marginal_player1_win_lose {
    val p1SkillFactor = new GaussianFactor(p1SkillVarId, 4, 81)
    val p2SkillFactor = new GaussianFactor(p2SkillVarId, 41, 25)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance, Vector(true, false))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)

    val iterNum = epCalibrate.calibrate(100, progress).iterNum
    assertEquals(2, iterNum)

    val outcomeMarginal = ep.marginal(outcomeVarId)
    assertEquals(0, outcomeMarginal.getValue((outcomeVarId, 0)), 0.00001)
    assertEquals(1, outcomeMarginal.getValue((outcomeVarId, 1)), 0.00001)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(25.5585, skill1Marginal.m, 0.0001)
    assertEquals(30.5421, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(34.7461, skill2Marginal.m, 0.0001)
    assertEquals(18.7043, skill2Marginal.v, 0.0001)
  }

  @Test def variable_marginal_player1_win_7_lose_3 {
    val p1SkillFactor = new GaussianFactor(p1SkillVarId, 4, 81)
    val p2SkillFactor = new GaussianFactor(p2SkillVarId, 41, 25)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance,
      Vector(false, false, false, true, true, true, true, true, true, true))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)

    val iterNum = epCalibrate.calibrate(100, progress).iterNum
    assertEquals(2, iterNum)

    val outcomeMarginal = ep.marginal(outcomeVarId)
    assertEquals(1, outcomeMarginal.getValue((outcomeVarId, 0)), 0.00001)
    assertEquals(0, outcomeMarginal.getValue((outcomeVarId, 1)), 0.00001)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(37.0968, skill1Marginal.m, 0.0001)
    assertEquals(13.028, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(27.3702, skill2Marginal.m, 0.0001)
    assertEquals(10.5800, skill2Marginal.v, 0.0001)
  }

  @Test def real_tennis_match {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

    val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

    val gameResults = TennisResult.fromMatches(filteredMatches, new Random(0))
    val pointResults = toPointResults(gameResults.head)

    val p1SkillFactor = new GaussianFactor(p1SkillVarId, 4, 81)
    val p2SkillFactor = new GaussianFactor(p2SkillVarId, 41, 25)
    val perfVariance = pow(250d / 6, 2)
    val tennisMatchFactor = TennisMatchFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance, pointResults)

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1SkillFactor)
    factorGraph.addFactor(p2SkillFactor)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph, threshold = 0.1)

    assertEquals(2, epCalibrate.calibrate(1000, progress).iterNum)

    val skill1Marginal = ep.marginal(p1SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(31.0654, skill1Marginal.m, 0.0001)
    assertEquals(32.2242, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2SkillVarId).asInstanceOf[GaussianFactor]
    assertEquals(30.3478, skill2Marginal.m, 0.0001)
    assertEquals(17.0686, skill2Marginal.v, 0.0001)
  }

  private def toPointResults(gameResult: TennisResult): IndexedSeq[Boolean] = {
    val pointResults = gameResult.points.get.map { point =>

      if (point.playerOnServe.equals(gameResult.player1))
        point.won else !point.won

    }
    pointResults.toVector
  }

  private def progress(currIter: Int) = println("EP iteration: " + currIter)
}