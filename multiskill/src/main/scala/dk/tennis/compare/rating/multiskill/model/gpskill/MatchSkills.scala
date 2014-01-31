package dk.tennis.compare.rating.multiskill.model.gpskill

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult

/**
 * @param result Tennis match result
 * @param player1Skills Skills at the beginning of the match
 * @param player2Skills Skills at the beginning of the match
 */
case class MatchSkills(tournament:TournamentResult,result:MatchResult,player1Skills:PlayerSkills,player2Skills:PlayerSkills)
