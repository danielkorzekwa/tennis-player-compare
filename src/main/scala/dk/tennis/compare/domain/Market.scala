package dk.tennis.compare.domain

import java.text.SimpleDateFormat
import java.util.Date

object Market {

  private val DATA_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS"
  
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

    markets.filter(m => m.runnerMap.size == 2).toList.sortWith((a,b) => a.scheduledOff.getTime() < b.scheduledOff.getTime())
  }

  private def mergeMarketRunners(markets: List[Market]): Map[Long, String] = markets.foldLeft(Map[Long, String]())((map, market) => map ++ market.runnerMap)
}

/**@param runnerMap[selectionId, selectionName]*/
case class Market(eventId: Long, fullDescription: String, scheduledOff: Date, runnerMap: Map[Long, String])
