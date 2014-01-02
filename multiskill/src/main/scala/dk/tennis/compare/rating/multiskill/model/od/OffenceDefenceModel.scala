package dk.tennis.compare.rating.multiskill.model.od

import java.util.Date
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait OffenceDefenceModel {

  def processGame(game: Game)

  /**
   * Returns the probability of winning a point for a future game. [player1ProbOnOffence,player2ProbOnOffence]
   */
  def pointProb(game:Game):Tuple2[Double, Double]

  def getSkill(player: String,opponent:String): PlayerSkills

  /**
   * @returns Map[playerName,playerSkills]
   */
  def getSkills(): Map[String, PlayerSkills]

}