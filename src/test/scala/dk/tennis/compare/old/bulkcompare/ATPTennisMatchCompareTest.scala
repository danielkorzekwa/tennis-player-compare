package dk.tennis.compare.old.bulkcompare

import org.junit._
import org.junit.Assert._
import dk.atp.api.facts.AtpFactsApi._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import org.joda.time.DateTime
import ATPTennisMatchCompareTest._
import dk.atp.api._
import dk.atp.api.tournament._
import ATPTennisMatchCompare._
import java.util.Date
import org.joda.time.format.ISODateTimeFormat
import dk.tennis.compare.old.bulkcompare.ATPTennisMatchCompare

object ATPTennisMatchCompareTest {

  private val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2010_2011.csv")

}

class ATPTennisMatchCompareTest {

  private val matchCompare = new ATPTennisMatchCompare(atpMatchesLoader)

  val marketTime2012 = new DateTime(0).withYear(2012).toDate()
  val marketTime2011 = new DateTime(0).withYear(2011).toDate()

  @Test def matchProb_Roger_Federer_vs_Milos_Raonic {

    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.855, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.144, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.907, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.797, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.8735, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }
  
   @Test def matchProb_Roger_Federer_vs_Milos_Raonic_discount_0_9_week_period {

     val matchCompare = new ATPTennisMatchCompare(atpMatchesLoader,12,0.9,7)
     
    val playerAFullName = "Roger Federer"
    val playerBFullName = "Milos Raonic"

    assertEquals(0.87, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.129, matchCompare.matchProb(playerBFullName, playerAFullName, CLAY, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.920, matchCompare.matchProb(playerAFullName, playerBFullName, CLAY, FIVE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.806, matchCompare.matchProb(playerAFullName, playerBFullName, GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.925, matchCompare.matchProb(playerAFullName, playerBFullName, HARD, THREE_SET_MATCH, marketTime2012), 0.001)

  }

  @Test def matchProb_not_enough_data_for_player_A {
    assertEquals(Double.NaN, matchCompare.matchProb("Milos Raonic", "Roger Federer", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
  }

  @Test def matchProb_not_enough_data_for_player_B {
    assertEquals(Double.NaN, matchCompare.matchProb("Roger Federer", "Milos Raonic", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
  }

  @Test  def matchProb_various_players_2011_2012 {
    assertEquals(0.464, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.806, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, marketTime2012), 0.001)
    assertEquals(0.704, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, marketTime2012), 0.001)

    assertEquals(0.064, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", CLAY, THREE_SET_MATCH, marketTime2011), 0.001)
    assertEquals(0.300, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", GRASS, THREE_SET_MATCH, marketTime2011), 0.001)
    assertEquals(0.377, matchCompare.matchProb("Novak Djokovic", "Rafael Nadal", HARD, THREE_SET_MATCH, marketTime2011), 0.001)

  }

  @Test  def matchProb_various_players2_2012 {
    assertEquals(0.9999, matchCompare.matchProb("Novak Djokovic", "Michael Russell", CLAY, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.991, matchCompare.matchProb("Novak Djokovic", "Michael Russell", GRASS, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9767, matchCompare.matchProb("Novak Djokovic", "Michael Russell", HARD, THREE_SET_MATCH, marketTime2012), 0.0001)

    assertEquals(0.9998, matchCompare.matchProb("Roger Federer", "Michael Russell", CLAY, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9951, matchCompare.matchProb("Roger Federer", "Michael Russell", GRASS, THREE_SET_MATCH, marketTime2012), 0.0001)
    assertEquals(0.9703, matchCompare.matchProb("Roger Federer", "Michael Russell", HARD, THREE_SET_MATCH, marketTime2012), 0.0001)

  }

  /**Tests for avgDiscount.*/
  private val dateTimeParser = ISODateTimeFormat.dateTimeParser().withZoneUTC()
  
  private def parseDate(date:String):DateTime = {
    DateTime.parse(date,dateTimeParser)
  }
  
  @Test def discountAvgNoData {
    val compare = new ATPTennisMatchCompare(atpMatchesLoader, 1, 7)
    val values: List[TimestampedDouble] = Nil
    assertEquals(Double.NaN, compare.avgDiscount(values), 0)
  }

  @Test def discountAvgNoDiscount {
    val compare = new ATPTennisMatchCompare(atpMatchesLoader,12,1, 7)
    val values: List[TimestampedDouble] = TimestampedDouble(parseDate("2012-02-12"), 6) :: TimestampedDouble(parseDate("2012-03-12"), 6) ::
      TimestampedDouble(parseDate("2012-04-12"), 4) :: TimestampedDouble(parseDate("2012-05-12"), 4) :: Nil
    assertEquals(5, compare.avgDiscount(values), 0)
  }

  @Test def discountAvgDiscount0_9_week_period {
    val compare = new ATPTennisMatchCompare(atpMatchesLoader,12, 0.9, 7)
    val values: List[TimestampedDouble] = TimestampedDouble(parseDate("2012-02-12"), 6) :: TimestampedDouble(parseDate("2012-03-12"), 6) ::
      TimestampedDouble(parseDate("2012-04-12"), 4) :: TimestampedDouble(parseDate("2012-05-12"), 4) :: Nil
    assertEquals(4.601, compare.avgDiscount(values), 0.001)
  }

  @Test def discountAvgDiscount0_9_month_period {
  
    
    val compare = new ATPTennisMatchCompare(atpMatchesLoader, 12,0.9, 30)
    val values: List[TimestampedDouble] = TimestampedDouble(parseDate("2012-02-12T-00:00"), 6) :: TimestampedDouble(parseDate("2012-03-12"), 6) ::
      TimestampedDouble(parseDate("2012-04-12"), 4) :: TimestampedDouble(parseDate("2012-05-12"), 4) :: Nil
    assertEquals(4.895, compare.avgDiscount(values), 0.001)
  }

  @Test def discountAvgDiscount0_9_year_period {
    val compare = new ATPTennisMatchCompare(atpMatchesLoader, 12,0.9, 365)
    val values: List[TimestampedDouble] = TimestampedDouble(parseDate("2012-02-12"), 6) :: TimestampedDouble(parseDate("2012-03-12"), 6) ::
      TimestampedDouble(parseDate("2012-04-12"), 4) :: TimestampedDouble(parseDate("2012-05-12"), 4) :: Nil
    assertEquals(5, compare.avgDiscount(values), 0.001)
  }

  implicit def toDate(dateTime: DateTime): Date = dateTime.toDate()

}