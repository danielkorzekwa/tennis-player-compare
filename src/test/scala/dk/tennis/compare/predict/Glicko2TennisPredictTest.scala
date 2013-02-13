package dk.tennis.compare.predict

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import dk.tennis.compare.predict.TennisPredict._
import scala.collection.mutable.ListBuffer
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConversions._
import java.text.SimpleDateFormat

class Glicko2TennisPredictTest {

  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  @Test def test {

    val tennishMatches = getTennisMatches()

    val matchFilter = (m: MatchComposite) => { new DateTime(m.tournament.tournamentTime.getTime()).getYear() >= 2008 }

    val predictionRecordsProgress: ListBuffer[PredictionRecord] = ListBuffer()
    val progress = (record: PredictionRecord) => {
      predictionRecordsProgress += record
      val llhProgress = calcLogLikelihood(predictionRecordsProgress) / predictionRecordsProgress.size
      println("Log likelihood= " + llhProgress)
    }
    val predictionRecords = Glicko2TennisPredict.predict(tennishMatches, matchFilter, progress)

    val csvReport = toCSVReport(predictionRecords.toList)
    FileUtils.writeLines(new File("./target/glicko2_match_level_report.csv"), csvReport)

  }

  private def toCSVReport(predictionRecords: List[PredictionRecord]): List[String] = {

    val csvReport = predictionRecords.map { r =>
      List(df.format(r.matchTime), r.playerA, r.playerB, r.playerAWinner, r.playerAWinnerProb).mkString(",")
    }.toList

    val header = "timestamp,playerA,playerB,playerAWin,playerAWinProb"

    header :: csvReport
  }

  private def getTennisMatches(): List[MatchComposite] = {
    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/atp_historical_data/match_data_2006_2011.csv");

    val matches = (2006 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val rand = new Random()
    val shuffledMatches = filteredMatches.map { m =>
      rand.nextBoolean match {
        case true => {
          val newMatchFacts = m.matchFacts.copy(playerAFacts = m.matchFacts.playerBFacts, playerBFacts = m.matchFacts.playerAFacts)
          m.copy(matchFacts = newMatchFacts)
        }
        case false => m
      }
    }

    shuffledMatches.toList
  }

}