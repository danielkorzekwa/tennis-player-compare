package dk.tennis.compare.rating.multiskill

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkills

trait MultiSkill {

  def processTennisMatch(tournament: TournamentResult, matchResult: MatchResult)

  def getSkills(player: String): MultiSkills

}