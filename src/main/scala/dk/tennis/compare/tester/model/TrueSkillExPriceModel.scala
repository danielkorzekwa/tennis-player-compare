package dk.tennis.compare.tester.model

import dk.atp.api.domain.MatchComposite
import org.slf4j.LoggerFactory
import scala.math._
import dk.atp.api.tournament.TournamentAtpApi._
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.simulation.game.GameResult
import dk.tennis.compare.simulation.game.TennisResult

case class TrueSkillExPriceModel(trueSkillModel: TrueSkillMatchModel, exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)
  private val glicko2Model = Glicko2MatchModel()
  private val pointStatsModel = PointStatsMatchModel()

  def gameProb(r: GameResult):Option[Double] = {
    val trueSkillProb = trueSkillModel.gameProb(r)
    val glicko2Prob = glicko2Model.gameProb(r)
    val pointStatsProb = pointStatsModel.gameProb(r)
    val exProb = exPricesModel.gameProb(r)

    if (r.containsPlayer("Roger Federer") || r.containsPlayer("Novak Djokovic")) {

      val federerTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Roger Federer")
      val djokovicTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Novak Djokovic")

      val federerGlicko = glicko2Model.glicko2.getRatings()("Roger Federer")
      val djokovicGlicko = glicko2Model.glicko2.getRatings()("Novak Djokovic")

      val federerFacts = PlayerFacts("Roger Federer", -1, -1)
      val djokovicFacts = PlayerFacts("Novak Djokovic", -1, -1)
      val testMatch = new TennisResult(player1="Roger Federer", player2="Novak Djokovic",numOfSets=r.asInstanceOf[TennisResult].numOfSets,timestamp=r.timestamp)
      val testMatchExProb = if (r.containsPlayer("Roger Federer") && r.containsPlayer("Novak Djokovic")) {
        if (r.player1.equals("Roger Federer")) exProb else Some(1 - exProb.getOrElse(-1d))
      } else None

      //federer djokovic
       println("%.2f,%.2f,%.2f %s".format(trueSkillModel.gameProb(testMatch).getOrElse(-1d), glicko2Model.gameProb(testMatch).getOrElse(-1d), testMatchExProb.getOrElse(-1d), r))

    }

    //  val probDiff = abs(1 - trueSkillProb.get / glicko2Prob.get)

    trueSkillProb
  }

  def addGameResult(r:GameResult) {
    glicko2Model.addGameResult(r)
    trueSkillModel.addGameResult(r)
    exPricesModel.addGameResult(r)
    pointStatsModel.addGameResult(r)
  }
}