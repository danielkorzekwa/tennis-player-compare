package dk.tennis.compare.game.tennis.model

import scala.util.Random
import org.slf4j.LoggerFactory
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

case class ExPriceCompareModel(exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)

  private val multiSkillModel = MultiSkillModel()

  private val random = new Random()
  private var profit = 0d
  private var betsCount = 0

  def gameProb(r: GameResult): Option[Double] = {
    //     val trueSkillProb = trueSkillModel.gameProb(r)
    val gameProb = multiSkillModel.gameProb(r)
    val exProb = exPricesModel.gameProb(r)

//    if (exProb.isDefined && abs(1 - (exProb.get / gameProb.get)) > 1) {
//
//      val player1Skill = multiSkillModel.getMultiSkillModel.getSkill(r.player1)
//      val player2Skill = multiSkillModel.getMultiSkillModel.getSkill(r.player2)
//      
//      val varThreshold = 0.2
//     if(player1Skill.skillOnServe.variance<varThreshold &&
//         player1Skill.skillOnReturn.variance<varThreshold &&
//         player2Skill.skillOnServe.variance<varThreshold &&
//         player2Skill.skillOnReturn.variance<varThreshold)
//      println(r.timestamp.get + ":" + exProb.get + ":" + gameProb + ":" + player1Skill + ":" + player2Skill)
//
//    }

    val player1 = "Roger Federer"
    val player2 = "Novak Djokovic"

    if (r.containsPlayer(player1) || r.containsPlayer(player2)) {
      val player1Skill = multiSkillModel.getMultiSkillModel.getSkill(player1)
      val player2Skill = multiSkillModel.getMultiSkillModel.getSkill(player2)

      val pointModel = GenericPointModel(multiSkillModel.multiSkillParams.perfVarianceOnServe, multiSkillModel.multiSkillParams.perfVarianceOnReturn)
      val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
      val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

      val directMatchProb = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
      else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

      val directExProb = if (r.containsPlayer(player1) && r.containsPlayer(player2)) {
        if (r.player1.equals(player1)) exProb.getOrElse(-1d) else 1 - exProb.getOrElse(-1d)
      } else Double.NaN

      println("%s, %s,%s, %.2f, %.2f, %s".format(r.timestamp.get, formatSkills(player1Skill), formatSkills(player2Skill), directExProb, directMatchProb, r))
    }
    gameProb

  }

  private def formatSkill(playerSkill: PlayerSkill): String = {
    "(%.2f,%.2f)".format(playerSkill.mean, playerSkill.variance)
  }

  private def formatSkills(playerSkills: PlayerSkills): String = {
    "%s, %s, %s".format(playerSkills.player, formatSkill(playerSkills.skillOnServe), formatSkill(playerSkills.skillOnReturn))
  }
  def addGameResult(r: GameResult) {
    //        trueSkillModel.addGameResult(r)
    multiSkillModel.addGameResult(r)
  }

  private def trading(predictedProb: Double, exProb: Double, winner: Boolean) {

    val (prob, exProbVal, win) = if (random.nextBoolean) (predictedProb, exProb, winner)
    else (1 - predictedProb, 1 - exProb, !winner)

    val probDiff = (1 - prob / exProbVal)

    if (probDiff > 0.05) {
      val price = 1 / exProbVal
      val betProfit = if (win) (price - 1) else -1
      profit += betProfit
      betsCount += 1
    }
  }
}