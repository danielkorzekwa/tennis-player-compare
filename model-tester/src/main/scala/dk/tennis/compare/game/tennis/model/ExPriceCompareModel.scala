package dk.tennis.compare.game.tennis.model

import scala.util.Random
import org.slf4j.LoggerFactory
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

case class ExPriceCompareModel(exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)

  private val multiSkillModel = MultiSkillModel()

  private val random = new Random()
  private var profit = 0d
  private var betsCount = 0

  def gameProb(tournament: TournamentResult, matchResult: MatchResult): Option[Double] = {
    //     val trueSkillProb = trueSkillModel.gameProb(r)
    val gameProb = multiSkillModel.gameProb(tournament, matchResult)
    val exProb = exPricesModel.gameProb(tournament, matchResult)

    // printModelVsExPrices(matchResult, gameProb, exProb)
    printPlayerReport(tournament, matchResult, gameProb, exProb)

    //    val player1 = "Roger Federer"
    //    val player2 = "Novak Djokovic"
    //
    //    if (matchResult.containsPlayer(player1) || matchResult.containsPlayer(player2)) {
    //      val player1Skill = multiSkillModel.getMultiSkillModel.getSkill(player1)
    //      val player2Skill = multiSkillModel.getMultiSkillModel.getSkill(player2)
    //
    //      val pointModel = GenericPointModel(multiSkillModel.multiSkillParams.perfVarianceOnServe, multiSkillModel.multiSkillParams.perfVarianceOnReturn)
    //      val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    //      val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)
    //
    //      val directMatchProb = if (matchResult.numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    //      else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)
    //
    //      val directExProb = if (matchResult.containsPlayer(player1) && matchResult.containsPlayer(player2)) {
    //        if (matchResult.player1.equals(player1)) exProb.getOrElse(-1d) else 1 - exProb.getOrElse(-1d)
    //      } else Double.NaN
    //
    //      println("%s, %s,%s, %.2f, %.2f, %s".format(tournament.tournamentTime, formatSkills(player1Skill), formatSkills(player2Skill), directExProb, directMatchProb, matchResult))
    //    }
    gameProb

  }

  private def printPlayerReport(tournament: TournamentResult, matchResult: MatchResult, gameProb: Option[Double], exProb: Option[Double]) {
    val player = "David Ferrer"

    if (matchResult.containsPlayer(player)) {

      val player1AceSkills = multiSkillModel.getMultiSkillModel.getSkills(matchResult.player1).aceSkills
      val player2AceSkills = multiSkillModel.getMultiSkillModel.getSkills(matchResult.player2).aceSkills

      val pointModel = GenericPointModel(multiSkillModel.multiSkillParams.perfVarianceOnServe, multiSkillModel.multiSkillParams.perfVarianceOnReturn)
      val p1AceProb = pointModel.pointProb(player1AceSkills.skillOnServe, player2AceSkills.skillOnReturn)
      val p2AceProb = pointModel.pointProb(player2AceSkills.skillOnServe, player1AceSkills.skillOnReturn)

      val directExProb = if (matchResult.player1.equals(player)) exProb.getOrElse(-1d) else 1 - exProb.getOrElse(-1d)
      val directGameProb = if (matchResult.player1.equals(player)) gameProb.getOrElse(-1d) else 1 - gameProb.getOrElse(-1d)

      println("%s,%.2f, %.2f, %s , %s, %s, %.2f, %.2f".format(
        tournament.tournamentTime, directGameProb, directExProb, matchResult, formatSkills(player1AceSkills), formatSkills(player2AceSkills),
        p1AceProb, p2AceProb))
    }
  }

  private def printModelVsExPrices(matchResult: MatchResult, gameProb: Option[Double], exProb: Option[Double]) {

    val playersA = List("John Isner", "Jo-Wilfried Tsonga")
    val playerA2 = List("Kevin Anderson", "Milos Raonic")

    val playersB = List("Fernando Verdasco", "Robin Haase")
    val playerB2 = List("Richard Gasquet", "David Ferrer")
    if ((playerB2.contains(matchResult.player1) || playerB2.contains(matchResult.player2)) && exProb.isDefined) {
      println(gameProb.get + "," + exProb.get)
    }
  }

  private def formatSkill(playerSkill: PlayerSkill): String = {
    "(%.2f,%.2f)".format(playerSkill.mean, playerSkill.variance)
  }

  private def formatSkills(playerSkills: PlayerSkills): String = {
    "%s, %s, %s".format(playerSkills.player, formatSkill(playerSkills.skillOnServe), formatSkill(playerSkills.skillOnReturn))
  }

  def addGameResult(tournament: TournamentResult, matchResult: MatchResult) = multiSkillModel.addGameResult(tournament, matchResult)

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