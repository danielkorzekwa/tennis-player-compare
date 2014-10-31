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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillovertime.SkillOverTimeCovFunc

class calcSkillsModelParamsTest extends Logging {

  //  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  //  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011) //.take(500)
  //
  //  val rand = new Random(456456)
  //  val opponentMap = Score.toScores(matchResults).map(s => s.player1.playerName).map { playerName =>
  //    playerName -> OpponentType(playerName, rand.nextBoolean)
  //  }.toMap

  val opponentMap = Map(
    "p1" -> OpponentType("p1", false), "p2" -> OpponentType("p2", false), "p3" -> OpponentType("p3", false), "p4" -> OpponentType("p4", false),
    "p5" -> OpponentType("p5", true), "p6" -> OpponentType("p6", true), "p7" -> OpponentType("p7", true), "p8" -> OpponentType("p8", true))
  val matchResults = generateMatches(opponentMap.keys.toList.sortBy(player => player), rounds = 1)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  logger.info("All matches:" + realScores.size / 2)
  logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

  logger.info("Simulating scores...")
  val scoresSimulator = ScoresSimulator()
  val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed = 5768658)

  val scores = simScores.map(s => s.score)
  // val scores = realScores

  @Test def test {

    //    println("Prior skills for all scores")
    //    simScores.take(20).foreach { s =>
    //      println(s.score.player1 + ":" + s.score.player2 + ":" + s.gameSkills.m.toArray.toList + ":" + s.gamePerfDiff.perfDiff + ":" + (1 - s.gamePerfDiff.perfDiff.cdf(0)))
    //    }

    logger.info("Learning parameters...")
    //    val skillCovParams = Array(log(0.3), log(30), log(1), log(365), 2.3)
    //    val skillCovFactory = SkillCovOverTimeFactory()

    val skillCovParams = Array(log(1), log(2),
      -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3)
    val skillCovFactory = OpponentSkillCovOverTimeFactory(scores)

    //      val skillCovParams = Array(log(1), log(2),
    //      log(0.3), log(30), log(1), log(365), 2.3)
    //    val skillCovFactory = OpponentSkillCovOverTimeFactory()

   // val priorSkillsGivenOpponent = SkillsGivenOpponent.sample(scores)

    val rand = new Random(444543)
    def getPlayerSkill(player: Player): PlayerSkill = PlayerSkill(rand.nextDouble * 0.1, player)
    val priorSkillCovFunc = skillCovFactory.create(skillCovParams.dropRight(1), getPlayerSkill)
    val priorModelParams = SkillsModelParams(scoresSimulator.skillMeanFunc, priorSkillCovFunc)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, skillCovFactory, scores, gradientMask = Array(1, 1, 0, 0, 0, 0, 0), progressListener,
      iterNum = 5)

  }

  private def progressListener(currParams: SkillsModelParams, covParamsLearn: Boolean) {

    //val players = opponentMap.keys.toList.sortBy(player => player)
    val playerNamesOffensive = opponentMap.values.filter(o => o.offensive).map(o => o.player).take(10).toList
    val playerNamesDefensive = opponentMap.values.filter(o => !o.offensive).map(o => o.player).take(10).toList
    val players = playerNamesOffensive ++ playerNamesDefensive

    currParams.skillCovFunc.asInstanceOf[OpponentOverTimeCovFunc].opponentOnReturnSimMatrix(players)

    val opponentOnReturnM = currParams.skillCovFunc.asInstanceOf[OpponentOverTimeCovFunc].opponentCovFunc.opponentOnReturnSimMatrix(players)
    println(opponentOnReturnM)

    val opponentOnServeM = currParams.skillCovFunc.asInstanceOf[OpponentOverTimeCovFunc].opponentCovFunc.opponentOnServeSimMatrix(players)
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

  case class OpponentSkillCovOverTimeFactory(scores: Seq[Score]) extends PlayerCovFuncFactory {
    def create(params: Seq[Double], getPlayerSkill: (Player) => PlayerSkill): CovFunc = {

      OpponentOverTimeCovFunc(params, scores, getPlayerSkill)
    }
  }

  case class SkillCovOverTimeFactory extends PlayerCovFuncFactory {
    def create(params: Seq[Double], getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
      SkillOverTimeCovFunc(params)
    }
  }

}