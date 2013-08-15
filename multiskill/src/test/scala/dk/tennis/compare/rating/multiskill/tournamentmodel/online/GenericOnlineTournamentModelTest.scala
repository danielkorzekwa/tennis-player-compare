package dk.tennis.compare.rating.multiskill.tournamentmodel.online

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PointResult

class GenericOnlineTournamentModelTest {

  val perfVarianceOnServe = 17
  val perfVarianceOnReturn = 15

  def initialPlayerSkills(playerName: String): PlayerSkills = playerName match {
    case "player1" => PlayerSkills("player1", PlayerSkill(0.1, 0.9290), PlayerSkill(-0.2, 0.9455))
    case "player2" => PlayerSkills("player2", PlayerSkill(0.2496, 0.9290), PlayerSkill(-0.1344, 0.9455))
    case "player3" => PlayerSkills("player3", PlayerSkill(0.5496, 0.9290), PlayerSkill(-0.2344, 0.9455))
    case "player4" => PlayerSkills("player4", PlayerSkill(0.7496, 0.9290), PlayerSkill(-0.3344, 0.9455))
  }

  val tournamentModel = GenericOnlineTournamentModel(initialPlayerSkills, perfVarianceOnServe, perfVarianceOnServe)

  val p1_vs_p2_result = MatchResult("player1", "player2", List(PointResult("player1", true), PointResult("player2", false), PointResult("player1", true)), true, 2)
  val p3_vs_p4_result = MatchResult("player3", "player4", List(PointResult("player4", false), PointResult("player4", true), PointResult("player3", false)), false, 2)
  val p1_vs_p4_result = MatchResult("player1", "player4", List(PointResult("player1", false), PointResult("player4", false), PointResult("player1", true)), true, 2)

  @Test def four_players_tournament {

    //semi final
    tournamentModel.onMatchResult(p1_vs_p2_result)
    tournamentModel.onMatchResult(p3_vs_p4_result)

    //final
    tournamentModel.onMatchResult(p1_vs_p4_result)

    val playerSkills = tournamentModel.getPlayerSkills()

    assertPlayerSkills(PlayerSkills("player1", PlayerSkill(0.3194, 0.8709), PlayerSkill(0.0707, 0.9132)), playerSkills("player1"), 0.0001)
    assertPlayerSkills(PlayerSkills("player2", PlayerSkill(0.1183, 0.9132), PlayerSkill(-0.3726, 0.9149)), playerSkills("player2"), 0.0001)
    assertPlayerSkills(PlayerSkills("player3", PlayerSkill(0.4109, 0.9129), PlayerSkill(-0.2039, 0.9140)), playerSkills("player3"), 0.0001)
    assertPlayerSkills(PlayerSkills("player4", PlayerSkill(0.5870, 0.8836), PlayerSkill(-0.1780, 0.8984)), playerSkills("player4"), 0.0001)
  }
}