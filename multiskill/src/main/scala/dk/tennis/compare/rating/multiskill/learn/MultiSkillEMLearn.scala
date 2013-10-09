package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.TournamentResult

/**
 * Learns multiskill parameters with Expectation Maximisation.
 *
 * @author Daniel Korzekwa
 *
 */
trait MultiSkillEMLearn {

  /**
   * Learns multiskill parameters with Expectation Maximisation.
   *
   * @return Learned MultiSkill parameters
   */
  def learn(multiSkillParams: MultiSkillParams, tournaments: Seq[TournamentResult], maxIter: Int, iterStatus: EMStatus => Unit): MultiSkillParams
}