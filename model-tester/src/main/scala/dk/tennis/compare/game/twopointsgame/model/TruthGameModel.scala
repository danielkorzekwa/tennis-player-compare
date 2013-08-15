package dk.tennis.compare.game.twopointsgame.model

import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult

case class TruthGameModel extends GameModel {

  def gameProb(r: GameResult): Option[Double] = r.trueWinProb

  def addGameResult(r: GameResult) = {

  }
}