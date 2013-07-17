package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.domain.PointResult

/**
 * Learns skill transition variance parameter with Expectation Maximisation.
 *
 * @author Daniel Korzekwa
 *
 */
trait MultiSkillEMLearn {

  /**
   * Learns skill transition variance parameter with Expectation Maximisation.
   *
   * @param skillTransVariance Initial variance for skill transition parameter
   * @param perfVariance Performance variance for both players
   * @param results Results of tennis matches
   * @param maxIter The number of iterations that EM learning is executed for
   *
   * @return Learned skill transition variance
   */
  def learn(skillTransVariance: Double, perfVariance: Double, results: Seq[PointResult], maxIter: Int): Double
}