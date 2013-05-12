package dk.tennis.compare.simulation.twopointsgame.player

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._

/**
 * Generate players.
 *
 * @author Daniel Korzekwa
 */
object GenericPlayerSimulator extends PlayerSimulator {

  def simulate(playersNum: Int, skillMean: Double, skillVariance: Double): Seq[Player] = {

    val normal = new NormalDistribution(skillMean, sqrt(skillVariance))

    val players = (1 to playersNum).map(i => Player("player" + i, normal.sample()))
    players
  }
}