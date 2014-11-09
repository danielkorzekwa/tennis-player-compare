package dk.tennis.compare

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.OpponentOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import java.util.Date

object OpponentCovAnalysis extends App{

   val opponentCovFunc = OpponentOverTimeCovFunc.fromFile("target/skillCovFunc").opponentCovFunc
   val players = opponentCovFunc.skillsOnServeGivenOpponent.keys.toList
   
    val p1 = Player("Roger Federer","Rafael Nadal",true,new Date(0))
  val opponentCov = players.map{p => 
      val p2 = Player("Roger Federer",p,true,new Date(0))
      
        val cov = opponentCovFunc.covariance(p1, p2)
      (p,cov)
   }.sortBy(cov => cov._2)
  
  
 
   
  opponentCov.foreach(println(_))
}