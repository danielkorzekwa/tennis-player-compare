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
  // private val priorProb = List(0.4939, 0.4751, 0.0310)
  // private val emissionProb = List(0.5772, 0.4228, 0.2799, 0.7201, 0.1077, 0.8923, 0.7260, 0.2740, 0.4666, 0.5334, 0.1876, 0.8124, 0.9916, 0.0084, 0.8272, 0.1728, 0.4742, 0.5258)
  // private val transitionProb = List(0.9929, 0.0069, 0.0002, 0.0105, 0.9801, 0.0094, 0.0002, 0.0014, 0.9984)

  //Num of prediction records=2550, logLikelihoodSum=-1487.287452, logLikelihoodAvg=-0.583250
  //  private val priorProb = List(0.6522, 0.2059, 0.1076, 0.0343)
  //  private val emissionProb = List(0.5037, 0.4963, 0.3639, 0.6361, 0.2110, 0.7890, 0.0629, 0.9371, 0.6388, 0.3612, 0.4867, 0.5133, 0.3905, 0.6095, 0.1125, 0.8875, 0.8906, 0.1094, 0.5336, 0.4664, 0.5291, 0.4709, 0.2255, 0.7745, 0.9468, 0.0532, 0.8106, 0.1894, 0.7339, 0.2661, 0.5591, 0.4409)
  //  private val transitionProb = List(0.9929, 0.0069, 0.0002, 0.0105, 0.9801, 0.0094, 0.0002, 0.0014, 0.9984)

  //Num of prediction records=2550, logLikelihoodSum=-1567.161656, logLikelihoodAvg=-0.614573, shuffle
  private val priorProb = List(0.3892, 0.2617, 0.1352, 0.0767, 0.0523, 0.0151, 0.0206, 0.0260, 0.0121, 0.0112)
  private val emissionProb = List(0.5301, 0.4699, 0.1651, 0.8349, 0.1990, 0.8010, 0.2573, 0.7427, 0.2011, 0.7989, 0.0345, 0.9655, 0.0179, 0.9821, 0.1296, 0.8704, 0.0359, 0.9641, 0.1238, 0.8762, 0.7206, 0.2794, 0.5202, 0.4798, 0.4554, 0.5446, 0.3547, 0.6453, 0.2841, 0.7159, 0.3277, 0.6723, 0.2030, 0.7970, 0.1085, 0.8915, 0.0242, 0.9758, 0.0799, 0.9201, 0.5933, 0.4067, 0.5768, 0.4232, 0.4778, 0.5222, 0.4392, 0.5608, 0.3126, 0.6874, 0.3429, 0.6571, 0.2988, 0.7012, 0.2776, 0.7224, 0.2076, 0.7924, 0.0987, 0.9013, 0.7659, 0.2341, 0.6758, 0.3242, 0.5803, 0.4197, 0.5116, 0.4884, 0.3363, 0.6637, 0.3053, 0.6947, 0.4415, 0.5585, 0.4564, 0.5436, 0.3695, 0.6305, 0.2110, 0.7890, 0.8908, 0.1092, 0.6985, 0.3015, 0.5455, 0.4545, 0.6107, 0.3893, 0.5473, 0.4527, 0.3431, 0.6569, 0.4746, 0.5254, 0.2763, 0.7237, 0.3456, 0.6544, 0.2040, 0.7960, 0.8298, 0.1702, 0.8112, 0.1888, 0.6997, 0.3003, 0.7191, 0.2809, 0.5130, 0.4870, 0.5962, 0.4038, 0.5473, 0.4527, 0.3838, 0.6162, 0.3300, 0.6700, 0.1435, 0.8565, 0.7703, 0.2297, 0.8682, 0.1318, 0.7222, 0.2778, 0.6480, 0.3520, 0.5503, 0.4497, 0.5916, 0.4084, 0.5972, 0.4028, 0.4941, 0.5059, 0.1996, 0.8004, 0.2259, 0.7741, 0.9035, 0.0965, 0.7333, 0.2667, 0.7206, 0.2794, 0.5784, 0.4216, 0.7968, 0.2032, 0.5905, 0.4095, 0.4131, 0.5869, 0.5225, 0.4775, 0.4189, 0.5811, 0.2853, 0.7147, 0.9904, 0.0096, 0.8903, 0.1097, 0.6688, 0.3312, 0.6617, 0.3383, 0.7311, 0.2689, 0.6801, 0.3199, 0.4776, 0.5224, 0.5026, 0.4974, 0.6620, 0.3380, 0.5058, 0.4942, 1.0000, 0.0000, 1.0000, 0.0000, 0.9050, 0.0950, 0.9200, 0.0800, 0.9110, 0.0890, 0.8237, 0.1763, 0.8293, 0.1707, 0.6343, 0.3657, 0.8101, 0.1899, 0.3872, 0.6128)
  private val transitionProb = List()

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

    predictionRecords.toList.sortWith((a, b) => a.matchTime.getTime < b.matchTime.getTime())

  }

  private def toResult(firstMatchTime: Long, m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val durationSinceFirstMatch = new Duration(timeDate.getMillis() - firstMatchTime).getStandardDays() / 30000

    val timeSlice = durationSinceFirstMatch.toInt

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }

  private implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
}