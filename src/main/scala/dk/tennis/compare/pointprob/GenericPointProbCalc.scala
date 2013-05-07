package dk.tennis.compare.pointprob

import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob.TennisProbFormulaCalc
import scala.math._

/**
 * Default implementation of the PointProbCalc.
 *
 * @author Daniel Korzekwa
 */
object GenericPointProbCalc extends PointProbCalc {

  def calcPointProb(matchProb: Double, matchType: MatchTypeEnum): Double = {
    var currProb = 0.3d
    var matched = false
    while (!matched && currProb < 0.7) {
      val matchProbGivenPointProb = TennisProbFormulaCalc.matchProb(currProb, currProb, matchType)
      matched = abs(matchProbGivenPointProb - matchProb) < 0.001
      if (!matched) currProb += 0.0001

    }

    if (matched) currProb else throw new IllegalArgumentException("Point probability not found")
  }
}