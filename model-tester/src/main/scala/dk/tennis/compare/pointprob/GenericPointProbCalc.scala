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

  private val precision = 0.001

  def calcPointProb(matchProb: Double, matchType: MatchTypeEnum): Double = {
    var currPointProb = 0d
    var prevError = 1d
    var step = 0.1
    var directionUp = true
    var matched = false

    while (!matched && currPointProb < 1) {
      if (directionUp) currPointProb += step else currPointProb -= step

      val matchProbGivenPointProb = TennisProbFormulaCalc.matchProb(currPointProb, currPointProb, matchType)
      val error = abs(matchProbGivenPointProb - matchProb)
      matched = error < precision

      if (error > prevError) {
        directionUp = !directionUp
        step = step / 2
      }
      prevError = error
    }

    if (matched) currPointProb else throw new IllegalArgumentException("Point probability not found")
  }
}