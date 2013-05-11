package dk.tennis.compare.tester.model

import dk.tennis.compare.tester.MatchModel
import dk.atp.api.domain.MatchComposite
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._

/**
 * Uses point stats from tennis match m to predict the outcome for the same match m.
 *
 * This model is created in order to check the correlation between point statistics and the outcome of the tennis game.
 *
 * @author Daniel Korzekwa
 */

case class PointStatsMatchModel extends MatchModel {

  def matchProb(m: MatchComposite): Option[Double] = {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    val playerAOnServeProb = playerAFacts.totalServicePointsWonPct
    val playerBOnServeProb = playerBFacts.totalServicePointsWonPct

    val matchProbAGivenB = if (m.tournament.numOfSet == 2) TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, FIVE_SET_MATCH)

    val tuned = 1 / (1 + exp(-10*(matchProbAGivenB-0.5)))
    Some(tuned)
  }

  def addMatchResult(m: MatchComposite) {

  }
}