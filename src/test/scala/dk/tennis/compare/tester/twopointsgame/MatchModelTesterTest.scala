package dk.tennis.compare.tester.twopointsgame

import org.joda.time.DateTime
import org.junit.Test
import org.slf4j.LoggerFactory
import dk.tennis.compare.game.tennis.model.TrueSkillGameModel
import dk.tennis.compare.game.twopointsgame.TwoPointsGame
import dk.tennis.compare.game.twopointsgame.model.TrueSkillPointModel
import dk.tennis.compare.game.twopointsgame.model.TruthGameModel
import dk.tennis.compare.game.twopointsgame.simulation.GenericGameSimulator
import dk.tennis.compare.game.twopointsgame.simulation.GenericPlayerSimulator
import dk.tennis.compare.tester.GameModelTester
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.twopointsgame.model.TrueSkillPointNotIddModel

class MatchModelTesterTest {

  val log = LoggerFactory.getLogger(getClass)

  val playersSimulator = GenericPlayerSimulator(Option(1))
  val players = playersSimulator.simulate(100, 0, 1).toIndexedSeq

  val perfVariance = (game: TwoPointsGame) => game match {
    case TwoPointsGame(0, 0) => (5d, 5d)
    case TwoPointsGame(1, 0) => (3d, 0.5d)
    case TwoPointsGame(0, 1) => (0.5d, 3d)
    case TwoPointsGame(1, 1) => (0.1d, 0.1d)
  }

  @Test def test {

    for (i <- 1 to 1) {

      val gamesSimulator = GenericGameSimulator(1)

      val gameResults = gamesSimulator.simulateGames(players, 2006, 2011, 1000, perfVariance)

      val tester = GameModelTester(gameResults)

      val matchFilter = (m: GameResult) => { /** log.info(m.toString); log.info("Log likelihood stats = " + tester.getLlhStats()); */ new DateTime(m.timestamp.get).getYear() >= 2010 }

      val truthModel = TruthGameModel()
      val trueSkillModel = TrueSkillGameModel()
      val trueSkillPointModel = TrueSkillPointModel()
      val trueSkillPointNotIddModel = TrueSkillPointNotIddModel()

      val modelSummary = tester.run(trueSkillPointNotIddModel, matchFilter)

      log.info("Log likelihood stats = " + modelSummary.llhStats)
      log.info("Expected/actual wins: %.3f/%s".format(modelSummary.playerAExpectedWins, modelSummary.playerActualWins))

      //  log.info(modelSummary.predictedActualAvgCorrReport)
    }
  }

}