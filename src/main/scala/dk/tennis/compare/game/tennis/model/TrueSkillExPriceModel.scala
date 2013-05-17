package dk.tennis.compare.game.tennis.model

import dk.atp.api.domain.MatchComposite
import org.slf4j.LoggerFactory
import scala.math._
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.domain.TennisResult

case class TrueSkillExPriceModel(trueSkillModel: TrueSkillMatchModel, exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)
  private val glicko2Model = Glicko2MatchModel()
  private val pointStatsModel = PointStatsMatchModel()
  private val trueSkillPointModel = TrueSkillPointModel()

  def gameProb(r: GameResult): Option[Double] = {
    val trueSkillProb = trueSkillModel.gameProb(r)
    val trueSkillPointProb = trueSkillPointModel.gameProb(r)
    val glicko2Prob = glicko2Model.gameProb(r)
    val pointStatsProb = pointStatsModel.gameProb(r)
    val exProb = exPricesModel.gameProb(r)

    if (r.containsPlayer("Roger Federer") && r.containsPlayer("Novak Djokovic")) {
      //
      //      val federerTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Roger Federer")
      //      val djokovicTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Novak Djokovic")
      //
      //      val federerGlicko = glicko2Model.glicko2.getRatings()("Roger Federer")
      //      val djokovicGlicko = glicko2Model.glicko2.getRatings()("Novak Djokovic")
      //
      //      val testMatch = new TennisResult(player1 = "Roger Federer", player2 = "Novak Djokovic", numOfSets = r.asInstanceOf[TennisResult].numOfSets, timestamp = r.timestamp, points = Nil)
      //      val testMatchExProb = if (r.containsPlayer("Roger Federer") && r.containsPlayer("Novak Djokovic")) {
      //        if (r.player1.equals("Roger Federer")) exProb else Some(1 - exProb.getOrElse(-1d))
      //      } else None

      //federer djokovic
      println("ts_point: %.2f ts: %.2f gl: %.2f ex: %.2f %s".format(trueSkillPointProb.getOrElse(-1d), trueSkillProb.getOrElse(-1d), glicko2Prob.getOrElse(-1d), exProb.getOrElse(-1d), r))

    }

    //  val probDiff = abs(1 - trueSkillProb.get / glicko2Prob.get)

    trueSkillPointProb
  }

  def addGameResult(r: GameResult) {
    //   glicko2Model.addGameResult(r)
    //   trueSkillModel.addGameResult(r)
    // trueSkillPointModel.addGameResult(r)
    //   exPricesModel.addGameResult(r)
    //   pointStatsModel.addGameResult(r)
  }
}