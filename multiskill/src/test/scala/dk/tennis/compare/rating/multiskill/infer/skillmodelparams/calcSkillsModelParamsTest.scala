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

class calcSkillsModelParamsTest extends Logging {

  val scores = getATPScores()
  val (simScores, scoresSimulator, opponentMap) = getATPSimulatedScores(randSeed=576576)
//  val (simScores, scoresSimulator, opponentMap) = getGeneratedScores(randSeed=576576)
  
//  println(opponentMap)
//    println("Prior skills for all scores")
//    simScores.take(2000).foreach { s =>
//      if (s.score.player1.playerName.equals("p1"))
//        println(s.score.player1 + ":" + s.score.player2 + ":" + s.gameSkills.m.toArray.toList + ":" + s.gamePerfDiff.perfDiff + ":" + (1 - s.gamePerfDiff.perfDiff.cdf(0)) + ":" + s.score)
//    }
//  
//  val scores = simScores.map(s => s.score)
  
  @Test def test {

    logger.info("Learning parameters...")

    val skillCovParams = Array(log(1), log(5), 2.3)


    val rand = new Random(444543)
    def getPlayerSkill(player: Player): PlayerSkill = PlayerSkill(Gaussian(scoresSimulator.skillMeanFunc(player) + rand.nextDouble *50.1,0), player)
    val priorSkillCovFunc = OpponentCovFunc(skillCovParams.dropRight(1), scores, getPlayerSkill(_))
//val priorSkillCovFunc = OpponentCovFunc.fromFile("target/Copy of skillCovFunc")
    
    val priorModelParams = SkillsModelParams(scoresSimulator.skillMeanFunc, priorSkillCovFunc)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, scores.toArray, gradientMask = Array(0, 0, 0), progressListener,
      iterNum = 50, skillCovParams.last)

  }

  private def progressListener(currParams: SkillsModelParams, covParamsLearn: Boolean) {

    //  val players = opponentMap.keys.toList.sortBy(player => player)
    val playerNamesOffensive = opponentMap.values.filter(o => o.offensive).map(o => o.player).toList.sorted.take(4).toList
    val playerNamesDefensive = opponentMap.values.filter(o => !o.offensive).map(o => o.player).toList.sorted.take(4).toList
//    val players = playerNamesOffensive ++ playerNamesDefensive
    val players=List("Roger Federer","Andy Murray","Omar Awadhy","Novak Djokovic","Rafael Nadal","Zhe Li")
   
    println(players)
    val opponentOnReturnM = currParams.skillCovFunc.asInstanceOf[OpponentCovFunc].opponentOnReturnSimMatrix(players)
    println(opponentOnReturnM)

    val opponentOnServeM = currParams.skillCovFunc.asInstanceOf[OpponentCovFunc].opponentOnServeSimMatrix(players)
    println(opponentOnServeM)

    if (!covParamsLearn) {

      currParams.skillCovFunc.save("target/skillCovFunc")
    }
  }

  private def getGeneratedScores(randSeed:Int): Tuple3[Seq[SimScore], ScoresSimulator, Map[String, OpponentType]] = {
    val offensivePlayers = (1 to 9).map(i => OpponentType("p" + i, true))
    val defensivePlayers = (10 to 19).map(i => OpponentType("p" + i, false))
    val opponentMap = (offensivePlayers ++ defensivePlayers).groupBy(o => o.player).mapValues(v => v.head)
    val matchResults = generateMatches(opponentMap.keys.toList.sortBy(player => player), rounds = 1)

    logger.info("Simulating scores")
    val realScores: Array[Score] = Score.toScores(matchResults)

    logger.info("All matches:" + realScores.size / 2)
    logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

    logger.info("Simulating scores...")
    val scoresSimulator = ScoresSimulator()
    val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed)

    (simScores, scoresSimulator, opponentMap)

  }

  private def getATPScores(): Seq[Score] = {
    val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
    val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011) //.take(500)

    val realScores: Array[Score] = Score.toScores(matchResults)

    logger.info("All matches:" + realScores.size / 2)
    logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

    realScores
  }

  private def getATPSimulatedScores(randSeed:Int): Tuple3[Seq[SimScore], ScoresSimulator, Map[String, OpponentType]] = {

    val realScores = getATPScores().toArray

    val rand = new Random(456456)
    val opponentMap = realScores.map(s => s.player1.playerName).map { playerName =>
      playerName -> OpponentType(playerName, rand.nextBoolean)
    }.toMap

    logger.info("Simulating scores...")
    val scoresSimulator = ScoresSimulator()
    val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, opponentMap, randSeed)

    val scores = simScores.map(s => s.score)
    
    (simScores,scoresSimulator,opponentMap)
  }
}
