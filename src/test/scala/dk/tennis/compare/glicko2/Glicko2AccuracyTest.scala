package dk.tennis.compare.glicko2

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import dk.tennis.compare.glicko2.Glicko2Rating.Result
import dk.tennis.compare.glicko2.Glicko2Rating._
import org.apache.commons.math.util.MathUtils
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import java.text.SimpleDateFormat
import java.util.Date

class Glicko2AccuracyTest {

  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  //Parameters for processing tennis match
  val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv")
  val glicko2 = new GenericGlicko2Rating(0, 100d / 173.7178, 0.06, 0.5, 14)
  val yearFrom = 2006
  val yearTo = 2011
  val minServicePoints = 20

  //Parameters for correlation summary report 
  val maxDeviation = 0.5
  val dateFrom = df.parse("2008-01-01 00:00:00")
  val dateTo = df.parse("2012-01-01 00:00:00")

  @Test def test {

    val results = glickoResults(yearFrom, yearTo, minServicePoints)

    val ratingsReports = ratingReports(results)

    val ratingsCSVReport = toCSVReport(ratingsReports)
    FileUtils.writeLines(new File("./target/glicko2_report.csv"), ratingsCSVReport)

    val correlationCSVReport = toCorrelationSummaryReport(ratingsReports, maxDeviation, dateFrom, dateTo)
    FileUtils.writeLines(new File("./target/glicko2_correlation_summary.csv"), correlationCSVReport)
  }

  /**Returns List[Tuple2[pointProbOnServe, averageScore]*/
  def toCorrelationSummaryReport(ratingReports: List[RatingsReport], maxDeviation: Double, dateFrom: Date, dateTo: Date): List[String] = {

    val filteredRatings = ratingReports
      .filter(r => r.currRatingA.ratingOnServe.deviation < maxDeviation && r.currRatingB.ratingOnReturn.deviation < maxDeviation
        && r.result.timestamp.getTime >= dateFrom.getTime && r.result.timestamp.getTime < dateTo.getTime)

    val correlationList: List[Tuple2[Double, Double]] = filteredRatings.map { r =>
      val playerAOnServeProb = GenericGlicko2Rating.E(r.currRatingA.ratingOnServe.rating, r.currRatingB.ratingOnReturn.rating, r.currRatingB.ratingOnReturn.deviation)
      round(playerAOnServeProb) -> r.result.score
    }

    val correlationReport = correlationList.groupBy { case (score, predictedScore) => score }.mapValues(r => (r.map(_._2).sum.toDouble / r.size))

    val correlationCSVHeader = "pointProbOnServe,avgScore"
    val correlationCSVReport = correlationReport.toList.sortWith((a, b) => a._1 < b._2).map(r => r._1 + "," + round(r._2))
    correlationCSVHeader :: correlationCSVReport
  }

  def toCSVReport(ratingReports: List[RatingsReport]): List[String] = {
    val ratingsCSVReport: List[String] = ratingReports.map { r =>

      val playerAOnServeProb = GenericGlicko2Rating.E(r.currRatingA.ratingOnServe.rating, r.currRatingB.ratingOnReturn.rating, r.currRatingB.ratingOnReturn.deviation)

      List(df.format(r.result.timestamp),
        r.result.playerA, r.result.playerB,
        round(r.currRatingA.ratingOnServe.rating), round(r.currRatingA.ratingOnServe.deviation), round(r.currRatingA.ratingOnServe.volatility), df.format(r.currRatingA.ratingOnServe.timestamp),
        round(r.currRatingB.ratingOnReturn.rating), round(r.currRatingB.ratingOnReturn.deviation), round(r.currRatingB.ratingOnReturn.volatility), df.format(r.currRatingB.ratingOnReturn.timestamp),
        round(playerAOnServeProb), round(r.result.score)).mkString(",")
    }
    val header = "timestamp,playerA,playerB," +
      "playerARatingOnServe,playerADeviationOnServe,playerAVolatilityOnServe,playerARatingTimestampOnServe," +
      "playerBRatingOnReturn,playerBDeviationOnReturn,playerBVolatilityOnReturn,playerBRatingTimestampOnReturn," +
      "playerAOnServeProb,playerARatioOfPointsWonOnServe"
    header :: ratingsCSVReport
  }

  def ratingReports(results: List[Result]): List[RatingsReport] = {
    val allRatingsReport: ListBuffer[RatingsReport] = ListBuffer()
    val summary: ListBuffer[Tuple2[Double, Double]] = ListBuffer()

    def onRatings(ratingsReport: RatingsReport): Unit = allRatingsReport += ratingsReport

    glicko2.calcServeReturnRatings(results, Option(onRatings))

    allRatingsReport.toList
  }

  def glickoResults(yearFrom: Int, yearTo: Int, minServicePoints: Int): List[Result] = {
    val matches = (yearFrom to yearTo).flatMap { year =>
      atpMatchesLoader.loadMatches(year).filter(m => m.tournament.surface.equals(HARD))
    }

    val glickoResults = matches.filter(m => m.matchFacts.playerAFacts.totalServicePoints > minServicePoints && m.matchFacts.playerBFacts.totalServicePoints > minServicePoints).flatMap { m =>
      val results =
        Result(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName,
          m.matchFacts.playerAFacts.totalServicePointsWonPct, m.tournament.tournamentTime) ::
          Result(m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.playerName,
            m.matchFacts.playerBFacts.totalServicePointsWonPct, m.tournament.tournamentTime) :: Nil

      results
    }

    glickoResults.toList
  }

  def round(value: Double): Double = MathUtils.round(value, 2)
}