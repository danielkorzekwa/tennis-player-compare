package dk.tennis.compare.rating.multiskill

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.util.Random
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import java.util.Date

// @TODO REfactor it to be offenseDefence model 
class GenericMultiSkillTest {
  //
  //  private val p1Skills = PlayerSkills("player1", PlayerSkill(2.0, 1), PlayerSkill(-2, 1))
  //  private val p2Skills = PlayerSkills("player2", PlayerSkill(5, 1), PlayerSkill(-3, 1))
  //
  //  val multiSkillParams = MultiSkillParams(
  //    skillOnServeTransVariance = 0.026103154972190633,
  //    skillOnReturnTransVariance = 0.06103154972158028,
  //    priorSkillOnServe = PlayerSkill(2.7100608379747073, 0.499569787325858), priorSkillOnReturn = PlayerSkill(-2.7100608379751296, 0.49956978732869395),
  //    perfVarianceOnServe = 100, perfVarianceOnReturn = 100)
  //
  //  val multiSkill = GenericMultiSkill(multiSkillParams)
  //  val pointModel = GenericPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)
  //
  //  @Test def converge_two_players {
  //
  //    val matchResult = MatchResult("player1", "player2", player1Won = true, numOfSets = 2, PlayerStats(7,70, 100), PlayerStats(5,64, 100))
  //    val tournamentResult = TournamentResult(new Date(0), "name", players = List(), List(matchResult))
  //
  //    for (i <- 1 to 100) multiSkill.processTennisMatch(tournamentResult, matchResult)
  //
  //    printReport()
  //
  //    multiSkill.processTennisMatch(tournamentResult, matchResult)
  //
  //    printReport()
  //  }
  //
  //  private def printReport() {
  //    val p1Skills = multiSkill.getSkills("player1").pointSkills
  //    val p2Skills = multiSkill.getSkills("player2").pointSkills
  //
  //    val p1PointProb = pointModel.pointProb(p1Skills.skillOnServe, p2Skills.skillOnReturn)
  //    val p2PointProb = pointModel.pointProb(p2Skills.skillOnServe, p1Skills.skillOnReturn)
  //
  //    println("p1 point prob=" + p1PointProb)
  //    println("p2 point prob=" + p2PointProb)
  //    println("p1 skills = " + p1Skills)
  //    println("p2 skills = " + p2Skills)
  //  }
}