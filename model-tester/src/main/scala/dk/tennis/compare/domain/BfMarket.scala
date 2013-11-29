package dk.tennis.compare.domain

import java.text.SimpleDateFormat
import java.util.Date
import BfMarket._

/**
 * Betfair market.
 *
 * @author Daniel Korzekwa
 */

/**@param runnerMap[selectionId, selection]*/
case class BfMarket(eventId: Long, fullDescription: String, scheduledOff: Date, runnerMap: Map[Long, Selection]) {
  val eventName = {
    val split = fullDescription.split("/")
    split(0).concat(split(1))
  }
}

object BfMarket {

  case class Selection(id: Long, name: String, price: Double, latestTaken: Date)

  private val DATA_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS"

  def fromCSV(marketData: List[String]): List[BfMarket] = {
    val df = new SimpleDateFormat(DATA_FORMAT)

    val singleRunnerMarkets = for {
      marketRecord <- marketData
      val marketRecordArray = marketRecord.split(",")

      val selectionId = marketRecordArray(3).toLong
      val selectionName = marketRecordArray(4)
      val price = try marketRecordArray(6).toDouble catch { case e: ArrayIndexOutOfBoundsException => -1d }
      val latestTaken = try df.parse(marketRecordArray(7)) catch { case e: ArrayIndexOutOfBoundsException => new Date(0) }
      val selection = Selection(selectionId, selectionName, price, latestTaken)
      val market = BfMarket(marketRecordArray(0).toLong, marketRecordArray(1), df.parse(marketRecordArray(2)), Map(selectionId -> selection))

    } yield market

    val markets = for {
      (marketId, marketRunners) <- singleRunnerMarkets.groupBy(m => m.eventId)
      val market = marketRunners(0).copy(runnerMap = mergeMarketRunners(marketRunners))
    } yield market

    markets.filter(m => m.runnerMap.size == 2).toList.sortWith((a, b) => a.scheduledOff.getTime() < b.scheduledOff.getTime())
  }

  private def mergeMarketRunners(markets: List[BfMarket]): Map[Long, Selection] = markets.foldLeft(Map[Long, Selection]())((map, market) => map ++ market.runnerMap)
}


