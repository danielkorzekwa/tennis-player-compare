package dk.tennis.compare.simulation.twopointsgame.player

import org.junit._
import Assert._

class GenericPlayerSimulatorTest {

  @Test def test {
    val players = GenericPlayerSimulator.simulate(100000, 2, 4)

    assertEquals(100000, players.size)
    assertEquals(2, players.map(_.skillMean).sum / players.size, 0.1)
  }
}