package dk.tennis.compare.rating.trueskill.learn

import dk.tennis.compare.rating.trueskill.model.Result

/**
 * Learns skill transition variance parameter with Expectation Maximisation.
 *
 * @author Daniel Korzekwa
 *
 */
trait TrueSkillEMLearn {

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
  def learn(skillTransVariance: Double, perfVariance: Double, results: Seq[Result], maxIter: Int): Double
}