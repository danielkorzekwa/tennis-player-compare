package dk.tennis.compare.rating.multiskill.analysis

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

object TransitivityCheck {

  /**Returns non-transitive triples of players*/
  def check(playerSkills: Seq[PlayerSkills]): Seq[Tuple3[PlayerSkills, PlayerSkills, PlayerSkills]] = {
    Nil
  }
}