package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

trait CovFunc {

  def withParams(newParams: Seq[Double]): CovFunc
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc

  def getParams(): Seq[Double]
  def covariance(player1: Player, player2: Player): Double
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double
  def save(file: String)

  //Implementations
  def covarianceMatrix(players: Array[Player]): Matrix = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-5
  def covarianceMatrix(playersA: Array[Player], playersB: Array[Player]): Matrix = Matrix(playersA.size, playersB.size, (rowIndex, colIndex) => covariance(playersA(rowIndex), playersB(colIndex)))

}