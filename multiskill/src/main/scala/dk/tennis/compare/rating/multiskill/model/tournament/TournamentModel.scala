package dk.tennis.compare.rating.multiskill.model.tournament

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait TournamentModel {

  /**
   * @param initialplayerSkills Player skills at the beginning of a tournament
   * @param tennisMatches Tournament tennis matches (in a time order)
   */
  def calcMatchSkills(initialPlayerSkills:Map[String,PlayerSkills],tennisMatches:Seq[MatchResult]):Seq[MatchSkills]
  
}