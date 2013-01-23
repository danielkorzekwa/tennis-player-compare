package dk.tennis.compare.glicko2

import org.junit._
import Assert._
import java.text.SimpleDateFormat
import Glicko2Rating._

class Glicko2SingleUpdateTest {

  @Test def test {

    val df = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss")

    val ratingPlayerAOnServe = Rating(rating = 0.52, deviation = 0.27, volatility = 0.025,
      timestamp = df.parse("03-Mar-2012 13:30:00"))

    val ratingplayerBOnReturn = Rating(rating = 0.2, deviation = 0.21, volatility = 0.04,
      timestamp = df.parse("06-Mar-2012 16:00:00"))

    val newRatingPlayerAOnServe = GenericGlicko2Rating.newRating(
      ratingPlayerAOnServe, ratingplayerBOnReturn, score = 0.7,
      tau = 0.5, df.parse("15-Mar-2012 13:30:00"), discountDurationInDays = 7)

    assertEquals(0.528, newRatingPlayerAOnServe.rating, 0.001)
    assertEquals(0.269, newRatingPlayerAOnServe.deviation, 0.001)
    assertEquals(0.024, newRatingPlayerAOnServe.volatility, 0.001)
  }
}