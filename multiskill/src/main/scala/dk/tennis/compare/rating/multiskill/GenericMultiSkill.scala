package dk.tennis.compare.rating.multiskill
import scala.collection._
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import java.util.Date
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkills
import dk.tennis.compare.rating.multiskill.model.od.GenericOffenceDefenceModel

case class GenericMultiSkill(multiSkillParams: MultiSkillParams) extends MultiSkill {

  private val pointModel = GenericOffenceDefenceModel(multiSkillParams)
  private val aceModel = GenericOffenceDefenceModel(multiSkillParams)

  def processTennisMatch(tournament: TournamentResult, matchResult: MatchResult) {

    val p1PointsOnServe = Tuple2(matchResult.p1Stats.servicePointsWon, matchResult.p1Stats.servicePointsTotal)
    val p2PointsOnServe = Tuple2(matchResult.p2Stats.servicePointsWon, matchResult.p2Stats.servicePointsTotal)

    pointModel.processGame(tournament.tournamentTime, matchResult.player1, matchResult.player2, p1PointsOnServe, p2PointsOnServe)

    val p1Aces = Tuple2(matchResult.p1Stats.aces, matchResult.p1Stats.servicePointsTotal)
    val p2Aces = Tuple2(matchResult.p2Stats.aces, matchResult.p2Stats.servicePointsTotal)
    aceModel.processGame(tournament.tournamentTime, matchResult.player1, matchResult.player2, p1Aces, p2Aces)

  }

  def getSkills(player: String): MultiSkills = {
    val pointSkills = pointModel.getSkill(player)
    val aceSkills = aceModel.getSkill(player)
    val multiSkills = MultiSkills(player, pointSkills, aceSkills)

    multiSkills
  }
}