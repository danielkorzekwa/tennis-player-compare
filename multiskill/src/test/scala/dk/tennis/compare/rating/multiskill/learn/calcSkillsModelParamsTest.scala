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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc
import dk.tennis.compare.rating.multiskill.scoresim.scoreSim
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent.OpponentOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent.OpponentOverTimeCovFunc

class calcSkillsModelParamsTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)//.take(100)

  logger.info("Simulating scores")
  val realScores: Array[Score] = Score.toScores(matchResults)

  logger.info("All matches:" + realScores.size / 2)
  logger.info("All players:" + realScores.map(s => s.player1.playerName).distinct.size)
  val trueSkillCovFactory = TrueSkillCovFactory(realScores)

  logger.info("Simulating scores...")
  val (scores, trueLoglik) = (realScores,Double.NaN)//simulateScores(realScores)

  @Test def test {

    logger.info("Learning parameters...")
    //  val skillCovParams = Array(log(1), log(0.4), log(0.6),
    //    log(0.3), log(30), log(1), log(365), 2.3)

    val skillCovParams = Array(log(1), log(0.4),
      log(0.3), log(30), log(1), log(365), 2.3)
    val skillCovFactory = OpponentSkillCovFactory()

    val priorSkillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player1))
    val priorSkillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player2))

    val priorModelParams = SkillsModelParams(
      skillPriorMeanOnServe = 0, skillPriorMeanOnReturn = 0,
      skillCovParams, priorSkillsOnServeGivenOpponent, priorSkillsOnReturnGivenOpponent)

    val skillsModelParams = calcSkillsModelParams(priorModelParams, skillCovFactory, scores)

    println("-----------------------")
    println("Prior skill cov params: " + priorModelParams.skillCovParams.toList)
    println("Learned skill cov params:" + skillsModelParams.skillCovParams.toList)

    println("Prior skill mean on serve/return: " + priorModelParams.skillPriorMeanOnServe + "/" + priorModelParams.skillPriorMeanOnReturn)
    println("New skill mean on serve/return: " + skillsModelParams.skillPriorMeanOnServe + "/" + skillsModelParams.skillPriorMeanOnReturn)

    val playerCovFunc = skillCovFactory.create(
      skillsModelParams.skillCovParams.dropRight(1),
      skillsModelParams.skillsOnServeGivenOpponent, skillsModelParams.skillsOnReturnGivenOpponent).asInstanceOf[OpponentOverTimeCovFunc]

    val playerNamesOffensive = trueSkillCovFactory.opponentMap.values.filter(o => o.offensive).map(o => o.player).take(10).toList
     val playerNamesDefensive = trueSkillCovFactory.opponentMap.values.filter(o => !o.offensive).map(o => o.player).take(10).toList
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

      val skills = playersGivenOpponent.map(p => PlayerSkill(rand.nextDouble*0.1, p.copy(opponentName = playerKey))).toSeq
      (playerKey, skills)
    }.toMap

    skillsGivenOpponentMap
  }

  /**
   * Returns simulated scores, total loglik
   */
  private def simulateScores(scores: Array[Score]): Tuple2[Array[Score], Double] = {

    val trueParams = DenseVector(log(1), log(0.4), log(0.6),
      log(0.3), log(30), log(1), log(365), 2.3)

    val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)
    val meanFunc = (player: Player) => { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }
    val covFunc = trueSkillCovFactory.create(trueParams.data.dropRight(1), Map(), Map())
    val simScores = scoreSim(scores, meanFunc, covFunc, logPerfStdDev = trueParams.data.last).map(s => s.score)

    val priorSkillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player1))
    val priorSkillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(scores.map(s => s.player2))
    val trueLoglik = SkillsDiffFunction(simScores, trueSkillMeanOnServe, trueSkillMeanOnReturn,
      priorSkillsOnServeGivenOpponent, priorSkillsOnReturnGivenOpponent, trueSkillCovFactory).calculate(trueParams)._1

    (simScores, trueLoglik)
  }

}

object calcSkillsModelParamsTest {

  case class TrueSkillCovFactory(scores: Seq[Score]) extends PlayerCovFuncFactory {

    val rand = new Random(456456)

    val opponentMap = scores.map(s => s.player1.playerName).map { playerName =>
      playerName -> OpponentType(playerName, rand.nextBoolean)
    }.toMap

    def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc = {
      OpponentTypeOverTimeCovFunc(params, opponentMap)
    }
  }

  case class OpponentSkillCovFactory extends PlayerCovFuncFactory {
    def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc = {
      OpponentOverTimeCovFunc(params, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
    }
  }

}