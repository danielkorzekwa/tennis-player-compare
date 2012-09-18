package dk.tennis.compare.predict
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.predict.TennisPredict.PredictionRecord
import dk.tennis.em.dbn.grmm.GrmmInferDbnTennisFactory
import dk.tennis.em.dbn.InferDbnTennis._
import org.joda.time.DateTime
import java.util.Date
import org.joda.time.Duration

/**
 * Predicts winner of a tennis match based on a dynamic bayesian network rating system.
 *
 * @author korzekwad
 */
object DbnTennisPredict extends TennisPredict {

  private val priorProb = List(0.2, 0.5, 0.3)

  private val emissionProb = List(
    0.5, 0.5,
    1d / 3, 2d / 3,
    0.25, 0.75,
    2d / 3, 1d / 3,
    0.5, 0.5,
    2d / 5, 3d / 5,
    3d / 4, 1d / 4,
    3d / 5, 2d / 5,
    0.5, 0.5)

  private val transitionProb = List(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  def predict(matches: Seq[MatchComposite], matchFilter: (MatchComposite) => Boolean): Seq[PredictionRecord] = {

    val matchesSize = matches.size

    val predictionRecords = for ((m, index) <- matches.zipWithIndex.filter(m => matchFilter(m._1))) yield {

      println("Predicting winner of tennis match  (DBN model) - %d / %d".format(index, matchesSize))

      val firstMatchTime = matches.head.tournament.tournamentTime.getTime()
      val results = matches.take(index + 1).map(m => toResult(firstMatchTime, m))

      val lastResult = results.last
      val inferenceResults = results.dropRight(1) :+ lastResult.copy(playerAWinner = None)
      val inferDbnTennis = new GrmmInferDbnTennisFactory().create(inferenceResults, priorProb, emissionProb, transitionProb)

      val matchProbAGivenB = inferDbnTennis.getPlayerAWinningProb(lastResult.playerA, lastResult.playerB, lastResult.timeSlice).max(0.0001).min(0.9999)

      val predictionRecord = PredictionRecord(
        m.tournament.tournamentTime, m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        matchProbAGivenB,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName))

      predictionRecord
    }

    predictionRecords

  }

  private def toResult(firstMatchTime: Long, m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val durationSinceFirstMatch = new Duration(timeDate.getMillis() - firstMatchTime).getStandardDays() / 3000

    val timeSlice = durationSinceFirstMatch.toInt

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }

  private implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
}