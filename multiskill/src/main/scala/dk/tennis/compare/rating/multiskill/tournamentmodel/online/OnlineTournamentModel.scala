package dk.tennis.compare.rating.multiskill.tournamentmodel.online

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait OnlineTournamentModel {

  def onMatchResult(matchResult: MatchResult)

  /**
   * @return Map[playerName,playerSkills]
   */
  def getPlayerSkills(): Map[String, PlayerSkills]

}