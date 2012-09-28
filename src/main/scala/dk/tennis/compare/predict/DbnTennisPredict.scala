package dk.tennis.compare.predict
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.predict.TennisPredict.PredictionRecord
import dk.tennis.em.dbn.infer.grmm.GrmmInferDbnTennisFactory
import dk.tennis.em.dbn.factorgraph.DbnTennis._
import org.joda.time.DateTime
import java.util.Date
import org.joda.time.Duration

/**
 * Predicts winner of a tennis match based on a dynamic bayesian network rating system.
 *
 * @author korzekwad
 */
object DbnTennisPredict extends TennisPredict {

  //Num of prediction records=2550, logLikelihoodSum=-1666.664724, logLikelihoodAvg=-0.653594
  //   private val priorProb = List(0.2, 0.5, 0.3) 
  //    private val emissionProb = List(
  //      0.5, 0.5,
  //      1d / 3, 2d / 3,
  //      0.25, 0.75,
  //      2d / 3, 1d / 3,
  //      0.5, 0.5,
  //      2d / 5, 3d / 5,
  //      3d / 4, 1d / 4,
  //      3d / 5, 2d / 5,
  //      0.5, 0.5)
  //    private val transitionProb = List(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  //Num of prediction records=2550, logLikelihoodSum=-1535.941386, logLikelihoodAvg=-0.602330
  //private val priorProb = List(0.5515, 0.3938, 0.0547)
  //private val emissionProb = List(0.5137, 0.4863, 0.2952, 0.7048, 0.0784, 0.9216, 0.7440, 0.2560, 0.5221, 0.4779, 0.2131, 0.7869, 0.9207, 0.0793, 0.7612, 0.2388, 0.4696, 0.5304)
  //private val transitionProb = List()

  //Num of prediction records=2550, logLikelihoodSum=-1439.444268, logLikelihoodAvg=-0.564488
  //private val priorProb = List(0.5572, 0.3961, 0.0467) 
  //private val emissionProb = List(0.4520, 0.5480, 0.2689, 0.7311, 0.0559, 0.9441, 0.7195, 0.2805, 0.5676, 0.4324, 0.1809, 0.8191, 0.9248, 0.0752, 0.7672, 0.2328, 0.5234, 0.4766)
  //private val transitionProb = List(0.9922, 0.0070, 0.0008, 0.0076, 0.9869, 0.0055, 0.0001, 0.0164, 0.9834)

  //Parameters learned from 2008-2009 data
  //Num of prediction records=2550, logLikelihoodSum=-1405.030290, logLikelihoodAvg=-0.550992, time slice = 3000
  private val priorProb = List(0.4939, 0.4751, 0.0310)
  private val emissionProb = List(0.5772, 0.4228, 0.2799, 0.7201, 0.1077, 0.8923, 0.7260, 0.2740, 0.4666, 0.5334, 0.1876, 0.8124, 0.9916, 0.0084, 0.8272, 0.1728, 0.4742, 0.5258)
  private val transitionProb = List(0.9929, 0.0069, 0.0002, 0.0105, 0.9801, 0.0094, 0.0002, 0.0014, 0.9984)

  def predict(matches: Seq[MatchComposite], matchFilter: (MatchComposite) => Boolean, progress: (PredictionRecord) => Unit): Seq[PredictionRecord] = {

    val matchesSize = matches.size

    val predictionRecords = for ((m, index) <- matches.zipWithIndex.filter(m => matchFilter(m._1))) yield {

      println("Predicting winner of tennis match  (DBN model) - %d / %d".format(index, matchesSize))

      val firstMatchTime = matches.head.tournament.tournamentTime.getTime()
      val results = matches.take(index + 1).map(m => toResult(firstMatchTime, m)).toList

      val lastResult = results.last
      val inferenceResults = results.dropRight(1) :+ lastResult.copy(playerAWinner = None)

      val inferDbnTennis = new GrmmInferDbnTennisFactory().create(inferenceResults, priorProb, emissionProb, transitionProb)

      val matchProbAGivenB = inferDbnTennis.getPlayerAWinningProb(lastResult.playerA, lastResult.playerB, lastResult.timeSlice)

      val predictionRecord = PredictionRecord(
        m.tournament.tournamentTime, m.matchFacts.playerAFacts.playerName,
        m.matchFacts.playerBFacts.playerName,
        matchProbAGivenB,
        m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName))

      progress(predictionRecord)

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