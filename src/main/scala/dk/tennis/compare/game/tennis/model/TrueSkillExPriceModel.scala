package dk.tennis.compare.game.tennis.model

import dk.atp.api.domain.MatchComposite
import org.slf4j.LoggerFactory
import scala.math._
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.domain.TennisResult
import scala.util.Random

case class TrueSkillExPriceModel(trueSkillModel: TrueSkillMatchModel, exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)
  private val glicko2Model = Glicko2MatchModel()
  private val pointStatsModel = PointStatsMatchModel()
  private val trueSkillPointModel = TrueSkillPointModel()
  private val trueSkillDbnModel = TrueSkillDBNMatchModel()

  private val random = new Random()
  private var profit = 0d
  private var betsCount = 0

  def gameProb(r: GameResult): Option[Double] = {
    val trueSkillProb = trueSkillModel.gameProb(r)
    val trueSkillDbnProb = trueSkillDbnModel.gameProb(r)
    val trueSkillPointProb = trueSkillPointModel.gameProb(r)
    val glicko2Prob = glicko2Model.gameProb(r)
    val pointStatsProb = pointStatsModel.gameProb(r)
    val exProb = exPricesModel.gameProb(r)

    if (r.containsPlayer("Roger Federer") && r.containsPlayer("Andy Murray")) {
      //
      val player1Skill = trueSkillModel.trueSkillModel.getRatings().get("Roger Federer")
      val player2Skill = trueSkillModel.trueSkillModel.getRatings().get("Andy Murray")

      val testMatch = new TennisResult(player1 = "Roger Federer", player2 = "Andy Murray", numOfSets = r.asInstanceOf[TennisResult].numOfSets, timestamp = r.timestamp, points = None)
      val testMatchExProb = if (r.containsPlayer("Roger Federer") && r.containsPlayer("Andy Murray")) {
        if (r.player1.equals("Roger Federer")) exProb else Some(1 - exProb.getOrElse(0d))
      } else None

      //println("%.2f,%.2f".format(trueSkillPointModel.gameProb(testMatch).getOrElse(-1d), testMatchExProb.getOrElse(-1d)))
      //  println("S1=%s S2=%s %s".format(player1Skill, player2Skill,r))
      println("rating=%s ts_point: %.2f ts: %.2f gl: %.2f ex: %.2f %s".format(player1Skill, trueSkillPointProb.getOrElse(-1d), trueSkillProb.getOrElse(-1d), glicko2Prob.getOrElse(-1d), exProb.getOrElse(-1d), r))
    }

    if (trueSkillProb.isDefined && exProb.isDefined) {

      //  trading(trueSkillPointProb.get, exProb.get, r.player1Win.get)
      //  if (r.containsPlayer("Roger Federer")) println(profit + "/" + betsCount + ":" + r)

      // println("%.2f,%.2f".format(trueSkillPointProb.get, exProb.get))
      trueSkillProb
    } else None

  }

  def addGameResult(r: GameResult) {
    // glicko2Model.addGameResult(r)
    trueSkillModel.addGameResult(r)
    // trueSkillDbnModel.addGameResult(r)
    // trueSkillPointModel.addGameResult(r)
    //  exPricesModel.addGameResult(r)
    //   pointStatsModel.addGameResult(r)
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