package dk.tennis.compare.rating.multiskill.model.od

import java.util.Date
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait OffenceDefenceModel {

  /**
   * @param timestamp The time of the game
   * @param player1
   * @param player2
   * @param p1PointsOnOffence [pointsWon,pointsTotal]
   * @param p2PointsOnOffence [pointsWon,pointsTotal]
   */
  def processGame(gameTime: Date, player1: String, player2: String, p1PointsOnOffence: Tuple2[Int, Int], p2PointsOnOffence: Tuple2[Int, Int])

  def getSkill(player: String): PlayerSkills
}