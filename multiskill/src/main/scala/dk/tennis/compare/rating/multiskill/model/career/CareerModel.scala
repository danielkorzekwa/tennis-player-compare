package dk.tennis.compare.rating.multiskill.model.career

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

trait CareerModel {

  /**
   * @param tournaments (in a time order)
   *
   * @returns Player skills at the beginning of tennis tournaments
   */
  def calcTournamentSkills(tournaments: Seq[TournamentResult]): Seq[TournamentSkills]
}