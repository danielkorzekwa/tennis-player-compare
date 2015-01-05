package dk.tennis.compare.domain

import org.junit._
import Assert._
import scala.io.Source

class filterBetfairDataTest {

  private val bfDataFileIn = "src/test/resources/raw_betfair_data_jan_2010/bfinf_other_100125to100131_100203122220.csv"
  private val bfDataDirIn = "src/test/resources/raw_betfair_data_jan_2010"
  private val bfMarketsFileOut = "target/bf_markets_tennis_jan_2010"

  @Test def test_file_in {

    filterBetfairData(bfDataFileIn, bfMarketsFileOut, bfRowFilter)

    val marketData = Source.fromFile(bfMarketsFileOut).getLines.drop(1).toList
    val markets = BfMarket.fromCSV(marketData)
    assertEquals(21, markets.size)
  }

  @Test def test_dir_in {

    filterBetfairData(bfDataDirIn, bfMarketsFileOut, bfRowFilter)

    val marketData = Source.fromFile(bfMarketsFileOut).getLines.drop(1).toList
    val markets = BfMarket.fromCSV(marketData)
    assertEquals(290, markets.size)
  }

  def bfRowFilter(bfRow: BetfairDataRow): Boolean = {
    bfRow.sportId == 2 &&
      bfRow.event.equals("Match Odds") &&
      !bfRow.fullDescription.contains("Women") &&
      bfRow.inPlay.equalsIgnoreCase("PE") &&
      bfRow.dateActualOff.get.getTime - bfRow.latestTaken.get.getTime > -1000L * 30 &&
      bfRow.dateActualOff.get.getTime - bfRow.latestTaken.get.getTime < 1000L * 1800
  }

}