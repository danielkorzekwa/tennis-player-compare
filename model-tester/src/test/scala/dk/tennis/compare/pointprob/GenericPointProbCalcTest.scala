package dk.tennis.compare.pointprob

import org.junit._

import Assert._
import dk.tennis.compare.pointprob.GenericPointProbCalc;
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob.TennisProbFormulaCalc
import TennisProbFormulaCalc._
class GenericPointProbCalcTest {

  @Test def three_sets_match {

    val pointProb = GenericPointProbCalc.calcPointProb(0.68, THREE_SET_MATCH)
    assertEquals(0.5173, pointProb, 0.0001)

    assertEquals(0.6799, TennisProbFormulaCalc.matchProb(pointProb, pointProb, THREE_SET_MATCH), 0.0001)
    assertEquals(0.7205, TennisProbFormulaCalc.matchProb(pointProb, pointProb, FIVE_SET_MATCH), 0.0001)
  }

  @Test def five_sets_match {

    val pointProb = GenericPointProbCalc.calcPointProb(0.68, FIVE_SET_MATCH)
    assertEquals(0.51389, pointProb, 0.0001)

    assertEquals(0.6454, TennisProbFormulaCalc.matchProb(pointProb, pointProb, THREE_SET_MATCH), 0.0001)
    assertEquals(0.6794, TennisProbFormulaCalc.matchProb(pointProb, pointProb, FIVE_SET_MATCH), 0.0001)
  }

}