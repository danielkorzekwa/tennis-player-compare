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

class calcSkillsModelParamsTest extends Logging {

  // val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  // val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)//.take(500)

  //val rand = new Random(456456)
  // val opponentMap = scores.map(s => s.player1.playerName).map { playerName =>
  //      playerName -> OpponentType(playerName, rand.nextBoolean)
  //    }.toMap

  val opponentMap = Map(
    "p1" -> OpponentType("p1", true), "p2" -> OpponentType("p2", true),
    "p3" -> OpponentType("p3", false), "p4" -> OpponentType("p4", false))
  val matchResults = generateMatches(opponentMap.keys.toList, rounds = 10)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  logger.info("All matches:" + realScores.size / 2)
  logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)

  logger.info("Simulating scores...")
  val scoresSimulator = ScoresSimulator()
  val (scores, trueLoglik) = scoresSimulator.simulate(realScores,opponentMap)

  @Test def test {

    logger.info("Learning parameters...")
    //  val skillCovParams = Array(log(1), log(0.4), log(0.6),
    //    log(0.3), log(30), log(1), log(365), 2.3)

    val skillCovParams = Array(log(1), log(2),
      log(0.3), log(30), log(1), log(365), 2.3)
    val skillCovFactory = OpponentSkillCovFactory()

    val priorSkillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player1))
    val priorSkillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player2))

    val priorModelParams = SkillsModelParams(
      skillPriorMeanOnServe = 0, skillPriorMeanOnReturn = 0,
      skillCovParams, priorSkillsOnServeGivenOpponent, priorSkillsOnReturnGivenOpponent)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, skillCovFactory, scores, gradientMask = Array(1, 1, 1, 1, 0, 0, 0), progressListener)

    println("-----------------------")
    println("Prior skill cov params: " + priorModelParams.skillCovParams.toList)
    println("Learned skill cov params:" + skillsModelParams.skillCovParams.toList)

    println("Prior skill mean on serve/return: " + priorModelParams.skillPriorMeanOnServe + "/" + priorModelParams.skillPriorMeanOnReturn)
    println("New skill mean on serve/return: " + skillsModelParams.skillPriorMeanOnServe + "/" + skillsModelParams.skillPriorMeanOnReturn)

  }

  private def progressListener(state: SkillDiffFuncState) {
    val playerCovFunc = OpponentCovFunc(Array(log(1), log(10)), state.newSkillsOnServeGivenOpponent, state.newSkillsOnReturnGivenOpponent)

    val playerNamesOffensive = opponentMap.values.filter(o => o.offensive).map(o => o.player).take(10).toList
    val playerNamesDefensive = opponentMap.values.filter(o => !o.offensive).map(o => o.player).take(10).toList
    val m = playerCovFunc.opponentOnReturnSimMatrix(playerNamesOffensive ::: playerNamesDefensive)

    println(m)
  }
  
  /**
   * Returns Map[opponent name, player skills against opponent]
   */
  def calcPriorSkillsGivenOpponent(playersGivenOpponent: Seq[Player]): Map[String, Seq[PlayerSkill]] = {

    val rand = new Random()
    val allPlayers = playersGivenOpponent.map(p => p.playerName).distinct

    val skillsGivenOpponentMap = allPlayers.map { playerKey =>

      val skills = playersGivenOpponent.map(p => PlayerSkill(rand.nextDouble * 0.1, p.copy(opponentName = playerKey))).toSeq
      (playerKey, skills)
    }.toMap

    skillsGivenOpponentMap
  }

 
}

object calcSkillsModelParamsTest {

 
  case class OpponentSkillCovFactory extends PlayerCovFuncFactory {
    def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc = {
      OpponentOverTimeCovFunc(params, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
    }
  }

}