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

class ATPTennisMatchBulkCompareTest {

  private val atpApi = new AtpWorldTourApiImpl()
  private val matchCompare = new ATPTennisMatchCompare(atpApi)

  private val atpBulkCompare = new ATPTennisMatchBulkCompare(matchCompare,2011, HARD, THREE_SET_MATCH)

  private val tennisMarketsFile = "src/test/resources/tennis_markets_single_market.csv"
  private val tennisProbFile = "./target/tennisprobfile_probabilities.csv"

  @Before
  def setup {
    new File(tennisProbFile).delete()
  }

  @Test def single_market {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb(tennisMarketsFile, tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,2263582,Igor Andreev,0.3434,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,4720522,Alexandr Dolgopolov,0.6566,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }

  @Test def two_markets {
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
  @Test def market_with_single_runner {
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
  @Test def market_with_no_probabilities {
    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_market_with_no_probabilities.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263684,Michael Berrer,0.7053,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270788,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Berrer v Sela,2011-01-03 00:30:00.000,2263685,Dudi Sela,0.2947,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }

  @Test def market_with_zero_probabilities_predictions_based_on_2010_year_data {

    val atpBulkCompare = new ATPTennisMatchBulkCompare(matchCompare,2010, HARD, THREE_SET_MATCH)

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb("src/test/resources/tennis_markets_zero_probability.csv", tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100277952,Group A/Australian Open 2011/Mens Tournament/First Round Matches/Nadal v Daniel,2011-01-18 01:15:00.000,2251410,Rafael Nadal,0.9998,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100277952,Group A/Australian Open 2011/Mens Tournament/First Round Matches/Nadal v Daniel,2011-01-18 01:15:00.000,2303581,Marcos Daniel,0.0002,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }
}