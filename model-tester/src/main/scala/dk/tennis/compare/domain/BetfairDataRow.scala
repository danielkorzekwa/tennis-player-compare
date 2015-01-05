package dk.tennis.compare.domain

import java.util.Date
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.text.ParseException

case class BetfairDataRow(sportId: Int, eventId: Int, fullDescription: String, scheduledOff: Option[Date], event: String, selectionId: Int, selection: String,
  dateActualOff: Option[Date], price: String, latestTaken: Option[Date], winFlag: String, inPlay: String) {

}

object BetfairDataRow {
  def apply(lineString:String): BetfairDataRow = {

   // val line = lineString.split(",")
    val line = lineString.split("\"").map(v => v.trim()).filter(v => !v.equals(",")).drop(1)
    val df1 = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
    df1.setTimeZone(TimeZone.getTimeZone("UTC"))
    val df2 = new SimpleDateFormat("dd-MM-yyyy HH:mm")
    df2.setTimeZone(TimeZone.getTimeZone("UTC"))

    def parse(dateString: String): Option[Date] = {
      try Some(df1.parse(dateString))
      catch {
        case e: ParseException => {
          try Some(df2.parse(dateString))
          catch { case e: ParseException => None }
        }
      }
    }
    try {
      val sportId = line(0).toInt
      val eventId = line(1).toInt
      val fullDescription = line(3)
      val scheduledOff = parse(line(4))
      val event = line(5)
      val selectionId = line(7).toInt
      val selection = line(8)
      val dateActualOff = parse(line(6))
      val price = line(9)
      val latestTaken = parse(line(12))
      val winFlag = line(14)
      val inPlay = line(15)

      BetfairDataRow(sportId, eventId, fullDescription, scheduledOff, event, selectionId, selection, dateActualOff, price, latestTaken, winFlag, inPlay)
    } catch {
      case e: Exception => throw new RuntimeException(line.toList.toString, e)
    }
  }

  private def dropQuotes(s: String): String = s//s.drop(1).dropRight(1)

}