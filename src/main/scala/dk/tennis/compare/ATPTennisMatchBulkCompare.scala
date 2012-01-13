package dk.tennis.compare

import scala.io.Source
import ATPTennisMatchBulkCompare._
/**
 * Calculates tennis market probabilities for a list of markets in a batch process.
 *
 * @author KorzekwaD
 */

object ATPTennisMatchBulkCompare {

  object Market {
    def fromCSV(marketData: Iterator[String]): List[Market] = Nil
  }
  case class Market

  case class MarketProb {
    def toCSV(): List[String] = {Nil}
  }
}
class ATPTennisMatchBulkCompare extends TennisMatchBulkCompare {

  /**
   * Calculates tennis market probabilities for a list of markets in a batch process and exports it to CSV file.
   *
   *  @param marketDataFileIn File path to CSV file with market records. Input market data CSV columns:
   *  market_id, market_name, market_time (yyyy-mm-dd hh:mm:ss, e.g. 2011-12-21 18:19:09),selection_id, selection_name
   *  There must be two records for every market, one record for each selection.
   *
   *  @param marketProbFileOut CVS file for exporting market probabilities.
   *  Columns: The same columns as for input file, and:  'probability' of winning a tennis match, surface (CLAY,GRASS,HARD), matchType (THREE_SET_MATCH/FIVE_SET_MATCH).
   *
   *  @param progress Current number of processed market.
   *
   */
  def matchProb(marketDataFileIn: String, marketProbFileOut: String, progress: (Int) => Unit): Unit = {
    val marketDataSource = Source.fromFile(marketDataFileIn)

    val markets = Market.fromCSV(marketDataSource.getLines())
    
    val marketProbabilities = markets.map(m => toMarketProb(m))
    
    marketProbabilities.foreach(prob => writeToFile(prob.toCSV()))
  }
  
    private def toMarketProb(m: Market): MarketProb = MarketProb()
    private def writeToFile(data: List[String]) {}
}