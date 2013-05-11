package dk.tennis.compare.tester.model

import dk.tennis.compare.tester.MatchModel
import dk.atp.api.domain.MatchComposite
import org.slf4j.LoggerFactory
import scala.math._
import dk.atp.api.tournament.TournamentAtpApi._

case class TrueSkillExPriceModel(trueSkillModel: TrueSkillMatchModel, exPricesModel: ExPricesMatchModel) extends MatchModel {

  private val log = LoggerFactory.getLogger(getClass)
  private val glicko2Model = Glicko2MatchModel()
  private val pointStatsModel = PointStatsMatchModel()

  def matchProb(m: MatchComposite): Option[Double] = {
    val trueSkillProb = trueSkillModel.matchProb(m)
    val glicko2Prob = glicko2Model.matchProb(m)
    val pointStatsProb = pointStatsModel.matchProb(m)
    val exProb = exPricesModel.matchProb(m)

    if ((m.matchFacts.containsPlayer("Roger Federer") || m.matchFacts.containsPlayer("Novak Djokovic"))) {

      val federerTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Roger Federer")
      val djokovicTrueSkill = trueSkillModel.getTrueSkillModel().getRatings()("Novak Djokovic")

      val federerGlicko = glicko2Model.glicko2.getRatings()("Roger Federer")
      val djokovicGlicko = glicko2Model.glicko2.getRatings()("Novak Djokovic")

      val federerFacts = PlayerFacts("Roger Federer", -1, -1)
      val djokovicFacts = PlayerFacts("Novak Djokovic", -1, -1)
      val testMatch = m.copy(matchFacts = MatchFacts(federerFacts, djokovicFacts, "?", "?", -1))
      val testMatchExProb = if (m.matchFacts.containsPlayer("Roger Federer") && m.matchFacts.containsPlayer("Novak Djokovic")) {
        if (m.matchFacts.playerAFacts.playerName.equals("Roger Federer")) exProb else Some(1 - exProb.getOrElse(-1d))
      } else None

      //federer djokovic
       println("%.2f,%.2f,%.2f %s".format(trueSkillModel.matchProb(testMatch).getOrElse(-1d), glicko2Model.matchProb(testMatch).getOrElse(-1d), testMatchExProb.getOrElse(-1d), m))

    }

    //  val probDiff = abs(1 - trueSkillProb.get / glicko2Prob.get)

    trueSkillProb
  }

  def addMatchResult(m: MatchComposite) {
    glicko2Model.addMatchResult(m)
    trueSkillModel.addMatchResult(m)
    exPricesModel.addMatchResult(m)
    pointStatsModel.addMatchResult(m)
  }
}