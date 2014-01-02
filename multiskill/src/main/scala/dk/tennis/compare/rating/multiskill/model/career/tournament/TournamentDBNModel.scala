package dk.tennis.compare.rating.multiskill.model.career.tournament

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult

trait TournamentDBNModel {

  /**
   *  Returns player skills given observed results of tennis tournament
   *
   *  @param beforeTSkills Player's skills at the beginning of a tennis tournament
   *  @param t Tournament results
   */
  def getAfterTSkills(): Map[String, PlayerSkills]
}