package dk.tennis.compare.domain

import java.text.SimpleDateFormat
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

/**@param runnerProbs[selectionId, probability]*/
case class MarketProb(market: Market, runnerProbs: Map[Long, Double], surface: SurfaceEnum, matchType: MatchTypeEnum) {

  private val DATA_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS"

  def toCSV(): List[String] = {
    val df = new SimpleDateFormat(DATA_FORMAT)
    val numberFormat = new java.text.DecimalFormat("#.####")
    val marketData = for {
      (selectionId, prob) <- runnerProbs
      val runnerRecord = market.eventId :: market.fullDescription :: df.format(market.scheduledOff) ::
        selectionId :: market.runnerMap(selectionId) :: numberFormat.format(prob) :: surface.toString :: matchType :: Nil

    } yield runnerRecord.mkString(",")

    marketData.toList
  }
}