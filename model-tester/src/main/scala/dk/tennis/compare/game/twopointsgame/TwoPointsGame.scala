package dk.tennis.compare.game.twopointsgame
import scala.util.Random

/**
 * Two points game. Who wins two points first, is the winner.
 *
 * @author Daniel Korzekwa
 */
case class TwoPointsGame(player1Score: Int, player2Score: Int) {

  def isFinished(): Boolean = player1Score == 2 || player2Score == 2

  def score(player1Win: Boolean): TwoPointsGame = {
    require(!isFinished(), "Game is finished")

    if (player1Win) TwoPointsGame(player1Score + 1, player2Score)
    else TwoPointsGame(player1Score, player2Score + 1)
  }

  /**
   * Returns the expected number of winning points for both players. Tuple2[player1PointsNum,player2PointsNum]
   *
   * @param player1PointWinProb Function that returns the probability of winning a point by player 1 given the current state of the game.
   */
  def expectedNumOfWinningPoints(player1PointWinProb: (TwoPointsGame) => Double): Tuple2[Double, Double] = {

    /**
     *  Returns the expected number of winning points for both players from the given state of the game till the end of the game.
     *  Tuple2[player1PointsNum,player2PointsNum]
     */
    def calcProb(gameState: TwoPointsGame, pointPlayedProb: Double): Tuple2[Double, Double] = {

      if (gameState.isFinished) (0, 0)
      else {
        val pointProb = player1PointWinProb(gameState)

        val expectedPointsWinBranch = calcProb(gameState.score(true), pointPlayedProb * pointProb)
        val expectedPointsLoseBranch = calcProb(gameState.score(false), pointPlayedProb * (1 - pointProb))

        val expectedPointsPlayer1 = pointPlayedProb * pointProb +
          expectedPointsWinBranch._1 + expectedPointsLoseBranch._1

        val expectedPointsPlayer2 = pointPlayedProb * (1 - pointProb) +
          expectedPointsWinBranch._2 + expectedPointsLoseBranch._2

        (expectedPointsPlayer1, expectedPointsPlayer2)
      }
    }

    val prob = calcProb(this, 1)
    prob
  }

  /**
   * Returns the probability of winning the game by player 1.
   *
   * @param player1PointWinProb Function that returns the probability of winning a point by player 1 given the current state of the game.
   */
  def gameProb(player1PointWinProb: (TwoPointsGame) => Double): Double = {

    val gameWinProb = this.isFinished() match {
      case true => if (player1Score == 2) 1 else 0

      case false => {
        val pointProb = player1PointWinProb(this)
        pointProb * this.score(true).gameProb(player1PointWinProb) +
          (1 - pointProb) * this.score(false).gameProb(player1PointWinProb)
      }
    }

    gameWinProb
  }

  /**
   * Returns simulated sequence of game points. Point is true if player 1 wins, otherwise it is false.
   *
   * @param player1PointWinProb Function that returns the probability of winning a point by player 1 given the current state of the game.
   * @param random
   */
  def simulate(player1PointWinProb: (TwoPointsGame) => Double, random: Random): Seq[Boolean] = {

    def sim(gameState: TwoPointsGame): List[Boolean] = {

      val points = gameState.isFinished() match {
        case true => Nil
        case false => {
          val sample = random.nextDouble
          val player1Win = sample < player1PointWinProb(gameState)
          player1Win :: sim(gameState.score(player1Win))
        }
      }

      points
    }

    sim(this)
  }
}

