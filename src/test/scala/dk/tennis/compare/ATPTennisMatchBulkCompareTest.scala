package dk.tennis.compare

import org.junit._
import Assert._
import java.io._
import scala.io._

class ATPTennisMatchBulkCompareTest {

  private val atpBulkCompare = new ATPTennisMatchBulkCompare()

  private val tennisMarketsFile = "src/test/resources/tennis_markets_single_market.csv"
  private val tennisProbFile = "./target/tennisProbFile_probabilities.csv"

  @Before
  def setup {
    new File(tennisProbFile).delete()
  }

  @Test def test {

    def progress(marketNumber: Int): Unit = {}

    atpBulkCompare.matchProb(tennisProbFile, "./target/tennis_markets_probabilities.csv", progress)

    val probSource = Source.fromFile(tennisProbFile)
    assertEquals(1, probSource.size)
  }
}