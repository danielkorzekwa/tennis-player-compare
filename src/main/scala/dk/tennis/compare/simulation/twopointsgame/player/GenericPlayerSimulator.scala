package dk.tennis.compare.simulation.twopointsgame.player

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._
import org.apache.commons.math3.random.Well19937c

/**
 * Generate players.
 *
 * @author Daniel Korzekwa
 */
case class GenericPlayerSimulator(seed: Option[Int]=None) extends PlayerSimulator {

  def simulate(playersNum: Int, skillMean: Double, skillVariance: Double): Seq[Player] = {

    val normal = seed match {
      case None => new NormalDistribution(skillMean, sqrt(skillVariance))
      case Some(seedValue) =>
        new NormalDistribution(new Well19937c(seedValue), skillMean, sqrt(skillVariance),
          NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
    }
    val players = (1 to playersNum).map(i => Player("player" + i, normal.sample()))
    players
  }
}