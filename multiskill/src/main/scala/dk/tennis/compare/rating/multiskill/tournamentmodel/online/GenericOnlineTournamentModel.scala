package dk.tennis.compare.rating.multiskill.tournamentmodel.online

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.collection._
import dk.tennis.compare.rating.multiskill.matchmodel.online.GenericOnlineMatchModel

/**
 * @param initialSkills Returns the initial player skills at the beginning of the tournament for the given player name
 */
case class GenericOnlineTournamentModel(initialSkills: (String) => PlayerSkills,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends OnlineTournamentModel {

  val latestPlayerSkills: mutable.Map[String, PlayerSkills] = mutable.Map()

  def onMatchResult(matchResult: MatchResult) = {

    val player1Skills = latestPlayerSkills.getOrElseUpdate(matchResult.player1, initialSkills(matchResult.player1))
    val player2Skills = latestPlayerSkills.getOrElseUpdate(matchResult.player2, initialSkills(matchResult.player2))

    require(player1Skills.player.equals(matchResult.player1), "Wrong player name")
    require(player2Skills.player.equals(matchResult.player2), "Wrong player name")

    val matchModel = GenericOnlineMatchModel(player1Skills, player2Skills, perfVarianceOnServe, perfVarianceOnReturn)

    matchResult.pointResults.foreach(point => matchModel.onPoint(point))

    val newPlayer1Skills = matchModel.getP1Skills()
    val newPlayer2Skills = matchModel.getP2Skills()

    latestPlayerSkills += matchResult.player1 -> newPlayer1Skills
    latestPlayerSkills += matchResult.player2 -> newPlayer2Skills

  }

  def getPlayerSkills(): immutable.Map[String, PlayerSkills] = latestPlayerSkills.toMap
}