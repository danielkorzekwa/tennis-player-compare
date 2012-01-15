package dk.tennis.compare

import org.junit._
import Assert._
import java.io._
import scala.io._

class ATPTennisMatchBulkCompareTest {

  private val atpBulkCompare = new ATPTennisMatchBulkCompare()

  private val tennisMarketsFile = "src/test/resources/tennis_markets_single_market.csv"
  private val tennisProbFile = "./target/tennisprobfile_probabilities.csv"

  @Before
  def setup {
    new File(tennisProbFile).delete()
  }

  @Test def test_single_market {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb(tennisMarketsFile, tennisProbFile, progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(3, probSource.getLines().size)

    assertEquals("event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type", probSource.reset().getLine(1))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,2263582,Igor Andreev,0.3434,HARD,THREE_SET_MATCH", probSource.reset().getLine(2))
    assertEquals("100270800,Group A/Brisbane International 2011/Mens Tournament/First Round Matches/Dolgopolov v Andreev,2011-01-03 05:45:00.000,4720522,Alexandr Dolgopolov,0.6566,HARD,THREE_SET_MATCH", probSource.reset().getLine(3))

  }
}