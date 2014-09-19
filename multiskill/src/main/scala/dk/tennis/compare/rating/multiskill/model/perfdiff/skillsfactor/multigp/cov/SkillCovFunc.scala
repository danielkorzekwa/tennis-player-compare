package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix

trait SkillCovFunc {

  def covarianceMatrix(players: Array[Player]): Matrix = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-5

  def covariance(player1: Player, player2: Player): Double
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double

  def getParams(): Seq[Double]

}