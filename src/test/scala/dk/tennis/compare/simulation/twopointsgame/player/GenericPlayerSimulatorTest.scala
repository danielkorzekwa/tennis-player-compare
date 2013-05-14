package dk.tennis.compare.simulation.twopointsgame.player

import org.junit._
import Assert._

class GenericPlayerSimulatorTest {

  @Test def test {
    val players = GenericPlayerSimulator(Some(1)).simulate(1000, 2, 4)

    assertEquals(1000, players.size)
    assertEquals(1.9965, players.map(_.skillMean).sum / players.size, 0.0001)
  }
}