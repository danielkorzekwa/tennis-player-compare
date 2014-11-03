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

class calcSkillsModelParamsTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011) //.take(500)

  val rand = new Random(456456)
  val opponentMap = Score.toScores(matchResults).map(s => s.player1.playerName).map { playerName =>
    playerName -> OpponentType(playerName, rand.nextBoolean)
  }.toMap

  //  val opponentMap = Map(
  //    "p1" -> OpponentType("p1", false), "p2" -> OpponentType("p2", false), "p3" -> OpponentType("p3", false), "p4" -> OpponentType("p4", false),
  //    "p5" -> OpponentType("p5", true), "p6" -> OpponentType("p6", true), "p7" -> OpponentType("p7", true), "p8" -> OpponentType("p8", true))
  //  val matchResults = generateMatches(opponentMap.keys.toList.sortBy(player => player), rounds = 1)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  logger.info("All matches:" + realScores.size / 2)
  logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

  logger.info("Simulating scores...")
  val scoresSimulator = ScoresSimulator()
  val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed = 5768658)

  // val scores = simScores.map(s => s.score)
  val scores = realScores

  @Test def test {

    logger.info("Learning parameters...")

    //    println("Prior skills for all scores")
    //    simScores.take(20).foreach { s =>
    //      println(s.score.player1 + ":" + s.score.player2 + ":" + s.gameSkills.m.toArray.toList + ":" + s.gamePerfDiff.perfDiff + ":" + (1 - s.gamePerfDiff.perfDiff.cdf(0)))
    //    }

    // val skillCovParams = Array(-0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3)
    // val priorSkillCovFunc = SkillOverTimeCovFunc(skillCovParams.dropRight(1))
    // priorSkillCovFunc.save("target/skillCovFunc")

    val skillCovParams = Array(log(1), log(2),
      -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3)

    val rand = new Random(444543)
    def getPlayerSkill(player: Player): PlayerSkill = PlayerSkill(rand.nextDouble * 0.1, player)
    val priorSkillCovFunc = OpponentOverTimeCovFunc(skillCovParams.dropRight(1), scores, getPlayerSkill)

    val priorModelParams = SkillsModelParams(scoresSimulator.skillMeanFunc, priorSkillCovFunc)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, scores, gradientMask = Array(1, 1, 0, 0, 0, 0, 0), progressListener,
      iterNum = 5, skillCovParams.last)

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

    if (!covParamsLearn) {
      currParams.skillCovFunc.save("target/skillCovFunc")
    }
  }
}
