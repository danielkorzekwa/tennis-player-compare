package dk.tennis.compare.rating.multiskill.factorgraph

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader

class GPTennisFactorGraphTest {

  val atpFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tournaments = MatchesLoader.loadTournaments(atpFile, 2011, 2011)

  val perfVariance = 150
 
 @Ignore @Test def test {
    println(tournaments.size)
    val gpFactorGraph = GPTennisFactorGraph(tournaments.take(20), perfVariance,skillOnServeMean=0,skillOnReturnMean=1)
    println(gpFactorGraph.calibrate(100))

    val skillGP = gpFactorGraph.getPlayerGPOnServeMarginal("Roger Federer")

    println(skillGP.getMean)
    println(skillGP.getVariance)
  }
  
  
}