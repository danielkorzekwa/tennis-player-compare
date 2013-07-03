package dk.tennis.compare.rating.trueskill.factorgraph.factor

import org.junit._
import org.junit.Assert._
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.util.Random
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate

class TennisMatchByPointFactorTest {

  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")

  val matches = (2011 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
  val filteredMatches = matches.filter(m => (m.tournament.surface == HARD) && m.matchFacts.playerAFacts.totalServicePointsWon > 10 && m.matchFacts.playerBFacts.totalServicePointsWon > 10)

  val gameResults = TennisResult.fromMatches(filteredMatches, new Random(0))

  val p1SkillVarId = 10
  val p2SkillVarId = 20
  val outcomeVarId = 30

  val perfVariance = pow(25d / 6, 2)

  val p1Skill = GaussianFactor(varId = p1SkillVarId, m = 4, v = 81)
  val p2Skill = GaussianFactor(varId = p2SkillVarId, m = 41, v = 25)

  @Ignore @Test def single_point_match_p1_wins {

    val tennisMatchFactor = TennisMatchByPointFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance,
      "p1", "p2", List(Result("p1", "p2", true)))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1Skill)
    factorGraph.addFactor(p2Skill)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)

    assertEquals(2, epCalibrate.calibrate(100, progress))

    val skill1Marginal = ep.marginal(p1Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(27.1744, skill1Marginal.m, 0.0001)
    assertEquals(37.4973, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(33.8473, skill2Marginal.m, 0.0001)
    assertEquals(20.8559, skill2Marginal.v, 0.0001)
  }

  @Ignore @Test def single_point_match_p2_wins {

    val tennisMatchFactor = TennisMatchByPointFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance,
      "p1", "p2", List(Result("p1", "p2", false)))

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1Skill)
    factorGraph.addFactor(p2Skill)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)

    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)
    assertEquals(2, epCalibrate.calibrate(100, progress))

    val skill1Marginal = ep.marginal(p1Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(3.9789, skill1Marginal.m, 0.0001)
    assertEquals(80.5513, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(41.0064, skill2Marginal.m, 0.0001)
    assertEquals(24.9572, skill2Marginal.v, 0.0001)
  }

  @Ignore @Test def match_p1_wins {

    val perfVariance = pow(250d / 6, 2)

    val tennisMatch = gameResults.head

    val pointResults = toPointResults(tennisMatch)

    val tennisMatchFactor = TennisMatchByPointFactor(p1SkillVarId, p2SkillVarId, outcomeVarId, perfVariance,
      tennisMatch.player1, tennisMatch.player2, pointResults)

    val factorGraph = GenericFactorGraph()
    factorGraph.addFactor(p1Skill)
    factorGraph.addFactor(p2Skill)
    factorGraph.addFactor(tennisMatchFactor)

    val ep = GenericEP(factorGraph)

    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)
    assertEquals(2, epCalibrate.calibrate(1000, progress))

    val skill1Marginal = ep.marginal(p1Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(30.1629, skill1Marginal.m, 0.0001)
    assertEquals(32.2376, skill1Marginal.v, 0.0001)

    val skill2Marginal = ep.marginal(p2Skill.varId).asInstanceOf[GaussianFactor]
    assertEquals(32.9250, skill2Marginal.m, 0.0001)
    assertEquals(17.0585, skill2Marginal.v, 0.0001)
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