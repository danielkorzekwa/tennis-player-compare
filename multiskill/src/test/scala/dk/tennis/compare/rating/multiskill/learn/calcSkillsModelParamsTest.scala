package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import breeze.linalg.DenseVector
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import calcSkillsModelParamsTest._
import scala.util.Random
import dk.tennis.compare.rating.multiskill.scoresim.scoreSim
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.matchloader.generateMatches
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.scoresim.ScoresSimulator
import dk.tennis.compare.rating.multiskill.infer.skillgivenskills.InferSkillGivenSkills
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.SkillsGivenOpponent
import dk.tennis.compare.rating.multiskill.scoresim.ScoresSimulator._
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.inferSkillsGivenOpponent
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.ThreadLocalRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Rand
import breeze.linalg.cholesky

class calcSkillsModelParamsTest extends Logging {

  // val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  // val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)//.take(500)

  //val rand = new Random(456456)
  // val opponentMap = scores.map(s => s.player1.playerName).map { playerName =>
  //      playerName -> OpponentType(playerName, rand.nextBoolean)
  //    }.toMap

  val opponentMap = Map(
    "p1" -> OpponentType("p1", false), "p2" -> OpponentType("p2", false), "p3" -> OpponentType("p3", false), "p4" -> OpponentType("p4", false),
    "p5" -> OpponentType("p5", true), "p6" -> OpponentType("p6", true), "p7" -> OpponentType("p7", false), "p8" -> OpponentType("p8", true))
  val matchResults = generateMatches(opponentMap.keys.toList.sortBy(player => player), rounds = 1)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  logger.info("All matches:" + realScores.size / 2)
  logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

  logger.info("Simulating scores...")
  val scoresSimulator = ScoresSimulator()
  val (scores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed = 5768658)

  @Test def test {

    println("Prior skills for all scores")
    scores.take(20).foreach { s =>
      println(s.score.player1 + ":" + s.score.player2 + ":" + s.gameSkills.m.toArray.toList + ":" + s.gamePerfDiff.perfDiff + ":" + (1 - s.gamePerfDiff.perfDiff.cdf(0)))
    }

    logger.info("Learning parameters...")
    val skillCovParams = Array(log(1), log(2),
      log(0.3), log(30), log(1), log(365), 2.3)
    val skillCovFactory = OpponentSkillCovFactory()

    val priorSkillsGivenOpponent = SkillsGivenOpponent.sample(scores.map(s => s.score))

    val priorModelParams = SkillsModelParams(
      scoresSimulator.skillMeanFunc,
      skillCovParams, priorSkillsGivenOpponent)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, skillCovFactory, scores.map(s => s.score), gradientMask = Array(1, 1, 0, 0, 0, 0, 0), progressListener,
      iterNum = 50)

  }

  private def progressListener(currParams: SkillsModelParams, covParamsLearn: Boolean) {
    val covParams = currParams.skillCovParams.take(2)
    val playerCovFunc = OpponentCovFunc(covParams, currParams.skillsGivenOpponent.skillsOnServeGivenOpponent, currParams.skillsGivenOpponent.skillsOnReturnGivenOpponent)

    val players = opponentMap.keys.toList.sortBy(player => player)
    val opponentOnReturnM = playerCovFunc.opponentOnReturnSimMatrix(players)
    println(opponentOnReturnM)
    
     val opponentOnServeM = playerCovFunc.opponentOnServeSimMatrix(players)
    println(opponentOnServeM)

//    if (!covParamsLearn) {
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p1").map(s => s.skill))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p2").map(s => s.skill))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p3").map(s => s.skill))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p4").map(s => s.skill))
//      
//       println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p1"))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p2"))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p3"))
//      println(currParams.skillsGivenOpponent.skillsOnServeGivenOpponent("p4"))
//    }
  }
}

object calcSkillsModelParamsTest {

  case class OpponentSkillCovFactory extends PlayerCovFuncFactory {
    def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc = {
      OpponentOverTimeCovFunc(params, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
    }
  }

}