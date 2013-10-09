package dk.tennis.compare.game.tennis.model

import dk.tennis.compare.tester.GameModel
import scala.math._
import dk.tennis.compare.tester.GameResult
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.GenericMultiSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.MultiSkill
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

case class MultiSkillModel extends GameModel {

  val multiSkillParams = MultiSkillParams(
    skillOnServeTransVariance = 0.03,
    skillOnReturnTransVariance = 0.03,
    priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
    perfVarianceOnServe = 200, perfVarianceOnReturn = 200)

  private val multiSkillModel = GenericMultiSkill(multiSkillParams)

  def gameProb(tournament: TournamentResult, r: MatchResult): Option[Double] = {
    val player1Skill = multiSkillModel.getSkills(r.player1).pointSkills
    val player2Skill = multiSkillModel.getSkills(r.player2).pointSkills

    val pointModel = GenericPointModel(multiSkillModel.multiSkillParams.perfVarianceOnServe, multiSkillModel.multiSkillParams.perfVarianceOnReturn)
    val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    val matchProb = if (r.asInstanceOf[MatchResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

    Some(matchProb)

  }

  def addGameResult(tournament: TournamentResult, matchResult: MatchResult) = multiSkillModel.processTennisMatch(tournament, matchResult)

  def getMultiSkillModel(): MultiSkill = multiSkillModel
}