package dk.tennis.compare

import scala.io.Source
import ATPTennisMatchBulkCompare._
import org.apache.commons.io.FileUtils._
import java.io.File
import scala.collection.JavaConversions._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import java.util.Date
import java.text.SimpleDateFormat
import dk.atp.api.AtpWorldTourApiImpl
import org.apache.commons.math.util._

/**
 * Calculates tennis market probabilities for a list of markets in a batch process.
 *
 * @author KorzekwaD
 */

object ATPTennisMatchBulkCompare {
  private val DATA_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS"

  object Market {
    
    def fromCSV(marketData: List[String]): List[Market] = {
      val df = new SimpleDateFormat(DATA_FORMAT)

      val singleRunnerMarkets = for {
        marketRecord <- marketData
        val marketRecordArray = marketRecord.split(",")
        val market = Market(marketRecordArray(0).toLong, marketRecordArray(1), df.parse(marketRecordArray(2)), Map(marketRecordArray(3).toLong -> marketRecordArray(4)))

      } yield market

      val markets = for {
        (marketId, marketRunners) <- singleRunnerMarkets.groupBy(m => m.eventId)
        val market = marketRunners(0).copy(runnerMap = mergeMarketRunners(marketRunners))
      } yield market

      markets.filter(m => m.runnerMap.size==2).toList
    }
    
    private def mergeMarketRunners(markets: List[Market]): Map[Long, String] = markets.foldLeft(Map[Long, String]())((map, market) => map ++ market.runnerMap)
  }
  /**@param runnerMap[selectionId, selectionName]*/
  case class Market(eventId: Long, fullDescription: String, scheduledOff: Date, runnerMap: Map[Long, String])

  /**@param runnerProbs[selectionId, probability]*/
  case class MarketProb(market: Market, runnerProbs: Map[Long, Double], surface: SurfaceEnum, matchType: MatchTypeEnum) {
    
    def toCSV(): List[String] = {
      val df = new SimpleDateFormat(DATA_FORMAT)
      val marketData = for {
        (selectionId, prob) <- runnerProbs
        val runnerRecord = market.eventId :: market.fullDescription :: df.format(market.scheduledOff) :: selectionId :: market.runnerMap(selectionId) :: MathUtils.round(prob, 4) :: surface :: matchType :: Nil
      } yield runnerRecord.mkString(",")

      marketData.toList
    }
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

    val markets = Market.fromCSV(marketDataSource.getLines().drop(1).toList)
    val marketsSize = markets.size
    val marketProbabilities = for ((market, index) <- markets.zipWithIndex) yield {
      progress(marketsSize - index)
      toMarketProb(market)
    }
    val marketProbsData = List.flatten(marketProbabilities.filter(p => p.runnerProbs.values.exists(!_.isNaN)).map(p => p.toCSV()))

    val marketProbFile = new File(marketProbFileOut)
    val header = "event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type"
    writeLines(marketProbFile, header :: marketProbsData)
  }

  private def toMarketProb(m: Market): MarketProb = {
    val atpApi = new AtpWorldTourApiImpl()
    val matchCompare = new ATPTennisMatchCompare(atpApi)
    val runners = m.runnerMap.keys.toList

    val surface = HARD
    val matchType = THREE_SET_MATCH

    val probability = try { matchCompare.matchProb(m.runnerMap(runners(0)), m.runnerMap(runners(1)), surface, matchType, 2011) } catch { case _ => Double.NaN }

    MarketProb(m, Map(runners(0) -> probability, runners(1) -> (1 - probability)), surface, matchType)
  }

}