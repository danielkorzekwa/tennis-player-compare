package dk.tennis.compare.rating.multiskill.sim

import dk.tennis.compare.rating.multiskill.domain.MatchResult

trait MatchSim {

  def sample():MatchResult
}