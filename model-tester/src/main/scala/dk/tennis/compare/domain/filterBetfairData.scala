package dk.tennis.compare.domain

import dk.bot.betfairservice.model.BFMarketData
import scala.io.Source
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConversions._
import java.text.SimpleDateFormat
import java.util.TimeZone
import com.typesafe.scalalogging.slf4j.Logging

object filterBetfairData extends Logging {

  def apply(bfDataFileIn: String, bfMarketsFileOut: String, bfRowFilter: (BetfairDataRow) => Boolean) {

    val bfDataFileInFile = new File(bfDataFileIn)
    val files = if (bfDataFileInFile.isDirectory()) FileUtils.listFiles(new File(bfDataFileIn), null, true).toList.map(f => f.getAbsolutePath())
    else List(bfDataFileIn)


    val bfRows = files.flatMap(f => filterFile(f, bfRowFilter))

    val sortedBfRows = bfRows.sortBy(r => (r.dateActualOff, r.eventId, r.selectionId))
    writeBfMarketsFile(sortedBfRows, bfMarketsFileOut)
  }

  private def filterFile(bfDataFileIn: String, bfRowFilter: (BetfairDataRow) => Boolean): Seq[BetfairDataRow] = {
    logger.info("Processing file:" + bfDataFileIn)
    val bfRows = Source.fromFile(bfDataFileIn).getLines.drop(1).map(l => BetfairDataRow(l)).filter { r =>
      bfRowFilter(r)
    }.toList

    val latestPrices = bfRows.groupBy(r => (r.eventId, r.selectionId)).mapValues(rows =>
      rows.sortBy(r => r.latestTaken.get.getTime).last)
      .values.toList

    latestPrices

  }
  private def writeBfMarketsFile(bfRows: Seq[BetfairDataRow], bfMarketsFileOut: String) {

    val df = new SimpleDateFormat("yyy-MM-dd HH:mm:ss.SSS")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    val header = "event_id,full_description,scheduled_off,selection_id,selection,dt_actual_off,price, LATEST_TAKEN,WIN_FLAG"
    val lines = bfRows.map { r =>
      "%d,%s,%s,%d,%s,%s,%s,%s,%s".format(
        r.eventId, r.fullDescription, df.format(r.scheduledOff.get), r.selectionId, r.selection, df.format(r.dateActualOff.get), r.price, df.format(r.latestTaken.get), r.winFlag)
    }.toList
    FileUtils.writeLines(new File(bfMarketsFileOut), header :: lines)

  }

}