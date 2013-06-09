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
  private val trueSkillPointServeReturnModel = TrueSkillPointServeReturnModel()
  private val trueSkillDbnModel = TrueSkillDBNMatchModel()

  private val random = new Random()
  private var profit = 0d
  private var betsCount = 0

  def gameProb(r: GameResult): Option[Double] = {
    //   val trueSkillProb = trueSkillModel.gameProb(r)
    //  val trueSkillDbnProb = trueSkillDbnModel.gameProb(r)
    // val trueSkillPointProb = trueSkillPointModel.gameProb(r)
    val trueSkillPointServeReturnProb = trueSkillPointServeReturnModel.gameProb(r)
    //    val glicko2Prob = glicko2Model.gameProb(r)
    //    val pointStatsProb = pointStatsModel.gameProb(r)
    //    val exProb = exPricesModel.gameProb(r)

    //    if (trueSkillProb.isDefined && r.containsPlayer("Roger Federer")) {
    //
    //      val player1SkillPoint = trueSkillPointServeReturnModel.trueSkillModel.getRatings().get("Roger Federer")
    //      val player1SkillMatch = trueSkillModel.trueSkillModel.getRatings().get("Roger Federer")
    //
    //      //  println("%.2f,%.2f,%.2f".format(player1SkillPoint.get._1.mean, player1SkillPoint.get._2.mean, player1SkillMatch.get.mean))
    //
    //      val player1Skill = trueSkillPointServeReturnModel.trueSkillModel.getRatings().get("Roger Federer")
    //      val player2Skill = trueSkillPointServeReturnModel.trueSkillModel.getRatings().get("Novak Djokovic")
    //
    //      val testMatch = new TennisResult(player1 = "Roger Federer", player2 = "Novak Djokovic", numOfSets = r.asInstanceOf[TennisResult].numOfSets, timestamp = r.timestamp, points = None)
    //      val testMatchExProb = if (r.containsPlayer("Roger Federer") && r.containsPlayer("Novak Djokovic")) {
    //        if (r.player1.equals("Roger Federer")) exProb else Some(1 - exProb.getOrElse(0d))
    //      } else None
    //
    //      //   println("%.2f,%.2f,%.2f,%.2f".format(player1Skill.get._1.mean,player1Skill.get._2.mean,player2Skill.get._1.mean,player2Skill.get._2.mean))
    //      //  println("%.2f,%.2f".format(trueSkillPointServeReturnModel.gameProb(testMatch).getOrElse(0d), testMatchExProb.getOrElse(0d)))
    //
    //      //   if (r.containsPlayer("Roger Federer") && r.containsPlayer("Novak Djokovic"))
    //      //   println("ts point_sr: %.2f ts_point: %.2f ts: %.2f gl: %.2f ex: %.2f %s".format(
    //      //     trueSkillPointServeReturnProb.getOrElse(-1d), trueSkillPointProb.getOrElse(-1d), trueSkillProb.getOrElse(-1d), glicko2Prob.getOrElse(-1d), exProb.getOrElse(-1d), r))
    //    }

    if (trueSkillPointServeReturnProb.isDefined) {

      //  trading(trueSkillPointProb.get, exProb.get, r.player1Win.get)
      //  if (r.containsPlayer("Roger Federer")) println(profit + "/" + betsCount + ":" + r)

      //     println("%.2f,%.2f".format(trueSkillProb.get, exProb.get))
      trueSkillPointServeReturnProb
    } else None

  }

  def addGameResult(r: GameResult) {
    //  glicko2Model.addGameResult(r)
    //  trueSkillModel.addGameResult(r)
    //   trueSkillDbnModel.addGameResult(r)
    //   trueSkillPointModel.addGameResult(r)
    trueSkillPointServeReturnModel.addGameResult(r)
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