package dk.tennis.compare.tester.model

import dk.atp.api.domain.MatchComposite
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._
import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.simulation.game.GameResult
import dk.tennis.compare.simulation.game.TennisResult
import dk.tennis.compare.simulation.game.TennisResult

/**
 * Uses point stats from tennis match m to predict the outcome for the same match m.
 *
 * This model is created in order to check the correlation between point statistics and the outcome of the tennis game.
 *
 * @author Daniel Korzekwa
 */

case class PointStatsMatchModel extends GameModel {

  def gameProb(r: GameResult): Option[Double] = {

    val tennisResult = r.asInstanceOf[TennisResult]

    val playerAOnServeProb = tennisResult.player1ServicePointsWonPct.get
    val playerBOnServeProb = tennisResult.player2ServicePointsWonPct.get

    val matchProbAGivenB = if (tennisResult.numOfSets == 2) TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, FIVE_SET_MATCH)

    val tuned = 1 / (1 + exp(-10 * (matchProbAGivenB - 0.5)))
    Some(tuned)
  }

  def addGameResult(r: GameResult) {}
}