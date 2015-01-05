package dk.tennis.compare

import dk.tennis.compare.domain.BetfairDataRow
import dk.tennis.compare.domain.filterBetfairData
import com.typesafe.scalalogging.slf4j.Logging

object filterBetfairDataApp extends App with Logging {

  //  private val bfDataFileIn = "c:/perforce/daniel/mult_skill/betfair_data/data/bfinf_other_100503to100509_100512122950.csv"
  private val bfDataDirIn = "c:/perforce/daniel/mult_skill/betfair_data/data/data_2013"
  private val bfMarketsFileOut = "c:/perforce/daniel/mult_skill/bf_markets_2013.csv"

  logger.info("Start filtring betfair files...")
  filterBetfairData(bfDataDirIn, bfMarketsFileOut, bfRowFilter)
  logger.info("Start filtring betfair files...DONE")

  def bfRowFilter(bfRow: BetfairDataRow): Boolean = {
    bfRow.sportId == 2 &&
      bfRow.event.equals("Match Odds") &&
      !bfRow.fullDescription.contains("Women") &&
      bfRow.inPlay.equalsIgnoreCase("PE") &&
      bfRow.dateActualOff.get.getTime - bfRow.latestTaken.get.getTime > -1000L * 30 &&
      bfRow.dateActualOff.get.getTime - bfRow.latestTaken.get.getTime < 1000L * 1800
  }
}