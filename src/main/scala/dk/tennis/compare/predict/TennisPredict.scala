package dk.tennis.compare.predict

import dk.atp.api.domain.MatchComposite
import java.util._
import TennisPredict._

/**Predicts tennis match winner for a set of matches.
 * */
object TennisPredict {
   case class PredictionRecord(matchTime: Date, playerA: String, playerB: String,
    playerAWinnerProb: Double,
    playerAWinner: Byte)
}
trait TennisPredict {

  /**Predicts tennis match winner for a set of matches.*/
  def predict(matches: Seq[MatchComposite]): Seq[PredictionRecord] 
}