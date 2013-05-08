package dk.tennis.compare.matching.playerspair

import org.junit._
import Assert._
import GenericPlayersPairMatcher._

class GenericPlayersPairMatcherTest {

  @Test def test {
    assertEquals(true, matchPlayersPair(("Roger Federer", "Novak Djokovic"), ("Roger Federer", "Novak Djokovic")))
    assertEquals(true, matchPlayersPair(("Roger Federer", "Novak Djokovic"), ("Novak Djokovic", "Roger Federer")))
    assertEquals(true, matchPlayersPair(("Roger FedereR", "NoVak Djokovic"), ("roger Federer", "NOvak Djokovic")))

    assertEquals(false, matchPlayersPair(("Roger Federer", "Novak Djokovic"), ("Roger Federer", "Novak Djokovic1")))
  }
}