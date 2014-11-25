package dk.tennis.compare

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.infer.matchprob.MatchPrediction
import dk.tennis.compare.domain.BfMarket
import scala.io.Source

case class InferMatchProbGivenGlicko2Best(glicko2ProbFile:String) {

  val matchProbs:Seq[Array[String]] = Source.fromFile(glicko2ProbFile).getLines.map(l => l.split(",")).toList
   def predict(matchResult: BfMarket,playerName:String): Double = {

    val matchProb = matchProbs.filter(l => l(0).toInt==matchResult.eventId && l(4).equals(playerName))
    
if(matchProb.size==1) matchProb(0)(5).toDouble else Double.NaN
  }
}