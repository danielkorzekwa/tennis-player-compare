package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import org.junit._
import Assert._
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.Gaussian

class OpponentCovFuncTest {

  @Test def test {

    val params = Array(log(10), log(0.01))
    val skillsOnServeGivenOpponentMap: Map[String, Seq[Gaussian]] = Map(
      "p1" -> Array(Gaussian(5,1)),
      "p2" -> Array(Gaussian(5,1)),
      "p3" -> Array(Gaussian(5.5,1)),
      "p4" -> Array(Gaussian(5.5,1)))
          
    val skillOnReturnGivenOpponentMap: Map[String, Seq[Gaussian]] = skillsOnServeGivenOpponentMap

    val cov = new OpponentCovFunc(params, Nil, skillsOnServeGivenOpponentMap, skillOnReturnGivenOpponentMap)

    val m = cov.opponentOnReturnSimMatrix(List("p1", "p2", "p3", "p4"))

    println(m)
  }

}