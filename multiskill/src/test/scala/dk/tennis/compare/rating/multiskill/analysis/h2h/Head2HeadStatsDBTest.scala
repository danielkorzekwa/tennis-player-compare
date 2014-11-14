package dk.tennis.compare.rating.multiskill.analysis.h2h

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import java.text.SimpleDateFormat

class Head2HeadStatsDBTest {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2012, 2014)

  val df = new SimpleDateFormat("dd/MM/yyyy")
  val timestamp = df.parse("14/11/2014")

  @Test def wawrinka_vs_cilic {
    val h2hDB = Head2HeadStatsDB(matchResults)

    val h2hStat = h2hDB.getH2HStat("Stan Wawrinka", "Marin Cilic", timestamp)

    assertEquals("Stan Wawrinka", h2hStat.player1)
    assertEquals("Marin Cilic", h2hStat.player2)
    assertEquals(2, h2hStat.p1Won)
    assertEquals(0, h2hStat.p2Won)
  }

  @Test def cilic_wawrinka {
    val h2hDB = Head2HeadStatsDB(matchResults)

    val h2hStat = h2hDB.getH2HStat("Marin Cilic", "Stan Wawrinka", timestamp)

    assertEquals("Marin Cilic", h2hStat.player1)
    assertEquals("Stan Wawrinka", h2hStat.player2)
    assertEquals(0, h2hStat.p1Won)
    assertEquals(2, h2hStat.p2Won)
  }

  @Test def federer_djokovic {
    val h2hDB = Head2HeadStatsDB(matchResults)

    val h2hStat = h2hDB.getH2HStat("Roger Federer", "Novak Djokovic", timestamp)

    assertEquals("Roger Federer", h2hStat.player1)
    assertEquals("Novak Djokovic", h2hStat.player2)
    assertEquals(5, h2hStat.p1Won)
    assertEquals(5, h2hStat.p2Won)
  }

  @Test def federer_djokovic_end_of_2013 {
    val h2hDB = Head2HeadStatsDB(matchResults)

    val timestamp = df.parse("31/12/2013")
    val h2hStat = h2hDB.getH2HStat("Roger Federer", "Novak Djokovic", timestamp)

    assertEquals("Roger Federer", h2hStat.player1)
    assertEquals("Novak Djokovic", h2hStat.player2)
    assertEquals(2, h2hStat.p1Won)
    assertEquals(3, h2hStat.p2Won)
  }
}