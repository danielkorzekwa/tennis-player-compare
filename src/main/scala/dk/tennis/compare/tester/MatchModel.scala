package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite

trait MatchModel {

  def matchProb(playerA:String,playerB:String,timestamp:Long):Option[Double]
  
  def addMatchResult(matchComposite:MatchComposite)
}