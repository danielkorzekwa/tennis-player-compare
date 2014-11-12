package dk.tennis.compare.rating.multiskill.infer.skillmodelparams

import scala.Array.canBuildFrom
import scala.math.log
import scala.util.Random
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.generateMatches
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.scoresim.ScoresSimulator
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.scoresim.ScoresSimulator
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentCovFunc
import dk.tennis.compare.rating.multiskill.scoresim.SimScore
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff

class calcSkillsModelParamsTest extends Logging {

  val scores = getATPScores()
  val (simScores, scoresSimulator, opponentMap) = getATPSimulatedScores(randSeed = 576576)
  //val (simScores, scoresSimulator, opponentMap) = getGeneratedScores(randSeed = 576576)
  //val scores = simScores.map(s => s.score)

  //  println("Prior skills for all scores")
  //  simScores.take(30).foreach { s =>
  //    if (s.score.player1.playerName.equals("p1"))
  //      println(s.score.player1 + ":" + s.score.player2 + ":" + s.gameSkills.m.toArray.toList + ":" + s.gamePerfDiff.perfDiff + ":" + (1 - s.gamePerfDiff.perfDiff.cdf(0)) + ":" + s.score)
  //  }

  @Test def test {

    logger.info("\nLearning parameters...")

    val skillCovParams = Array(
      log(1), // opponent 
      log(1), log(1), log(1), // surface
      log(1), log(30), log(1), log(365) //time
      )
    val priorSkillCovFunc = SkillCovFunc(skillCovParams)
    val priorModelParams = SkillsModelParams(scoresSimulator.skillMeanFunc, priorSkillCovFunc)

    val logPerfStdDev = 2.3
    val skillsModelParams = calcSkillsModelParams(priorModelParams, scores.toArray, gradientMask = Array(1, 1, 1, 1, 1, 1, 1, 1, 0), progressListener,
      iterNum = 10, logPerfStdDev)

    println("")
    logger.info("Final params: %s".format(skillsModelParams.skillCovFunc.getParams.toString))

    // val p1SkillsOnServe = scores.map(s => s.player1).filter(p => p.playerName.equals("p1")).toArray
    // val p1CovMatrix = skillsModelParams.skillCovFunc.covarianceMatrix(p1SkillsOnServe)
    // println(p1CovMatrix)

  }

  private def progressListener(state: SkillDiffFuncState) {

    println("")
    logger.info("params: %s".format(state.skillCovFunc.getParams.toString))

    val playerOnServe = Player("p1", "p2", onServe = true, new Date(0), Surface.HARD)
    val playerOnReturn = Player("p1", "p2", onServe = false, new Date(0), Surface.HARD)
    logger.info("New mean on serve/return: %.2f/%.2f".format(state.skillMeanFunc(playerOnServe), state.skillMeanFunc(playerOnReturn)))

    logger.info("loglik: %.2f, d: %s,".format(state.logLik, state.loglikD.toList))

    val hardPerfLoglik = -OutcomeLik.totalLoglik(state.perfDiffs.map(p => p.perfDiff), scores.toArray, score => { score.player1.surface == Surface.HARD })

    logger.info("hard loglik: %.2f".format(hardPerfLoglik))

  }

  private def getGeneratedScores(randSeed: Int): Tuple3[Seq[SimScore], ScoresSimulator, Map[String, OpponentType]] = {
    val offensivePlayers = (1 to 9).map(i => OpponentType("p" + i, true))
    val defensivePlayers = (10 to 19).map(i => OpponentType("p" + i, false))
    val opponentMap = (offensivePlayers ++ defensivePlayers).groupBy(o => o.player).mapValues(v => v.head)
    val matchResults = generateMatches(opponentMap.keys.toList.sortBy(player => player), rounds = 1, randSeed)

    logger.info("\nSimulating scores")
    val realScores: Array[Score] = Score.toScores(matchResults)

    logger.info("All matches:" + realScores.size / 2)
    logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

    logger.info("Simulating scores...")
    val scoresSimulator = ScoresSimulator()
    val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed)
    logger.info("True loglik: %.2f".format(trueLoglik))

    (simScores, scoresSimulator, opponentMap)

  }

  private def getATPScores(): Seq[Score] = {
    val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
    val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2013) //.take(500)

    val realScores: Array[Score] = Score.toScores(matchResults)

    logger.info("All matches:" + realScores.size / 2)
    logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

    realScores
  }

  private def getATPSimulatedScores(randSeed: Int): Tuple3[Seq[SimScore], ScoresSimulator, Map[String, OpponentType]] = {

    val realScores = getATPScores().toArray

    val rand = new Random(456456)
    val opponentMap = realScores.map(s => s.player1.playerName).map { playerName =>
      playerName -> OpponentType(playerName, rand.nextBoolean)
    }.toMap

    logger.info("Simulating scores...")
    val scoresSimulator = ScoresSimulator()
    val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed)

    val scores = simScores.map(s => s.score)

    logger.info("True loglik: %.2f".format(trueLoglik))
    (simScores, scoresSimulator, opponentMap)
  }
}
