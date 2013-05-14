package dk.tennis.compare.game.twopointsgame.simulation

import org.junit.Assert.assertEquals
import org.junit.Test
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import dk.tennis.compare.game.twopointsgame.TwoPointsGame

class GenericGameSimulatorTest {

  val gamesSimulator = GenericGameSimulator(1)

  val playersSimulator = GenericPlayerSimulator(Option(1))

  val perfVariance = (game: TwoPointsGame) => (1d, 1d)

  @Test def simulate {

    val players = playersSimulator.simulate(100, 0, 1).toIndexedSeq
    val gameResults = gamesSimulator.simulateGames(players, 2006, 2008, 2, perfVariance)

    assertEquals(6, gameResults.size)

    //assert result 1
    assertEquals(None, gameResults(0).eventName)
    assertEquals("player86", gameResults(0).player1)
    assertEquals("player89", gameResults(0).player2)
    assertEquals(true, gameResults(0).player1Win.get)
    assertEquals(0.6795, gameResults(0).trueWinProb.get, 0.0001)
    assertEquals(new DateTime("2006-01-01T01:01:00", DateTimeZone.UTC), new DateTime(gameResults(0).timestamp.get, DateTimeZone.UTC))
    assertEquals(List(true, true), gameResults(0).points)
    assertEquals(0.6005, gameResults(0).player1ExpectedPointProb, 0.0001)

    //assert result 5
    assertEquals(None, gameResults(4).eventName)
    assertEquals("player99", gameResults(4).player1)
    assertEquals("player54", gameResults(4).player2)
    assertEquals(false, gameResults(4).player1Win.get)
    assertEquals(0.6795, gameResults(4).trueWinProb.get, 0.0001)
    assertEquals(new DateTime("2008-01-01T01:01:00", DateTimeZone.UTC), new DateTime(gameResults(4).timestamp.get, DateTimeZone.UTC))
    assertEquals(List(true, false, false), gameResults(4).points)
    assertEquals(0.6005, gameResults(4).player1ExpectedPointProb, 0.0001)
  }
}