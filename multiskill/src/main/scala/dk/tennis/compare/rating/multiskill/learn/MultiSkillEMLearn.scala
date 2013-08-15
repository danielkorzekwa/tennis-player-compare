package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams

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
  def learn(multiSkillParams: MultiSkillParams, results: Seq[MatchResult], maxIter: Int, iterStatus: EMStatus => Unit): MultiSkillParams
}