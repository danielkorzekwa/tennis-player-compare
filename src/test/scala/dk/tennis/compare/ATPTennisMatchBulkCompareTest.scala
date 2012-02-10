package dk.tennis.compare

import org.junit._
import Assert._
import java.io._
import scala.io._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.collection.mutable.ListBuffer
import dk.atp.api.AtpWorldTourApiImpl
import dk.atp.api._
import TournamentAtpApi._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import domain._
import ATPTennisMatchBulkCompareTest._

object ATPTennisMatchBulkCompareTest {

  private var tournamentApi: GenericTournamentAtpApi = new GenericTournamentAtpApi(5000)
  private val atpMatchesLoader = new CachedATPMatchesLoader(tournamentApi)

}

class ATPTennisMatchBulkCompareTest {

  private val matchCompare = new ATPTennisMatchCompare(atpMatchesLoader)

  private val atpBulkCompare = new ATPTennisMatchBulkCompare(matchCompare, atpMatchesLoader)

  private val tennisMarketsFile = "src/test/resources/tennis_markets_single_market.csv"
  private val tennisProbFile = "./target/tennisprobfile_probabilities.csv"

  @Before
  def setup {
    new File(tennisProbFile).delete()
  }

  @Test @Ignore def single_market {
  
    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb(tennisMarketsFile, tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,2263582,Igor Andreev,0.3434,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,4720522,Alexandr Dolgopolov,0.6566,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }

  @Test @Ignore def two_markets {
    val progressUpdate: ListBuffer[Int] = ListBuffer()
    def progress(marketNumber: Int): Unit = progressUpdate += marketNumber

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_two_markets.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(5, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263684,Michael Berrer,0.7053,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263685,Dudi Sela,0.2947,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,2263582,Igor Andreev,0.3434,HARD,THREE_SET_MATCH", probSource.reset().getLine(4))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,4720522,Alexandr Dolgopolov,0.6566,HARD,THREE_SET_MATCH", probSource.reset().getLine(5))

    assertEquals(List(2, 1), progressUpdate.toList)
  }

  //market with single runner should be ignored
  @Test @Ignore def market_with_single_runner {
    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_market_with_single_runner.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(5, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-02 05:45:00.000,2263582,Igor Andreev,0.3434,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-02 05:45:00.000,4720522,Alexandr Dolgopolov,0.6566,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263684,Michael Berrer,0.7053,HARD,THREE_SET_MATCH", probSource.reset().getLine(4))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263685,Dudi Sela,0.2947,HARD,THREE_SET_MATCH", probSource.reset().getLine(5))

  }

  //market with no probabilities should be ignored
  @Test @Ignore def market_with_no_probabilities {
    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_market_with_no_probabilities.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263684,Michael Berrer,0.7053,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263685,Dudi Sela,0.2947,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }

  @Test @Ignore def market_with_zero_probabilities_predictions_based_on_2010_year_data {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_zero_probability.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100277952,London / Queen's Club/Nadal v Daniel,2010-06-07 01:15:00.000,2251410,Rafael Nadal,0.993,GRASS,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100277952,London / Queen's Club/Nadal v Daniel,2010-06-07 01:15:00.000,2303581,Marcos Daniel,0.007,GRASS,THREE_SET_MATCH", probSource.reset().getLine(3))

  }
  
   @Test @Ignore def market_grass {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_grass.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("102994153,Group A/Wimbledon 2011/Mens Tournament/First Round Matches/Soderling v Petzschner,2011-06-21 15:15:00.000,2263597,Robin Soderling,0.0945,GRASS,FIVE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("102994153,Group A/Wimbledon 2011/Mens Tournament/First Round Matches/Soderling v Petzschner,2011-06-21 15:15:00.000,2525241,Philipp Petzschner,0.9055,GRASS,FIVE_SET_MATCH", probSource.reset().getLine(3))

  }
   
    @Test @Ignore def market_clay {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_clay.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("102860782,Group A/French Open 2011/Mens Tournament/First Round Matches/Ferrer v Nieminen,2011-05-22 10:15:00.000,2257304,Jarkko Nieminen,0.0197,CLAY,FIVE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("102860782,Group A/French Open 2011/Mens Tournament/First Round Matches/Ferrer v Nieminen,2011-05-22 10:15:00.000,2257314,David Ferrer,0.9803,CLAY,FIVE_SET_MATCH", probSource.reset().getLine(3))

  }
}