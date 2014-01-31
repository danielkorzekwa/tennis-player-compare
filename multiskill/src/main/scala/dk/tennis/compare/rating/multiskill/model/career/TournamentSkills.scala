package dk.tennis.compare.rating.multiskill.model.career

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

/**
 * @param tournament
 * @param initialPlayerSkills Player skills at the beginning of a tournament
 */
case class TournamentSkills(tournament:TournamentResult,initialPlayerSkills:Map[String,PlayerSkills])