package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import breeze.linalg.DenseMatrix

trait CovFunc {

  def withParams(newParams: Seq[Double]): CovFunc
  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc

  def getParams(): Seq[Double]
  def covariance(player1: Player, player2: Player): Double
  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double
  def save(file: String)

  //Implementations
  def covarianceMatrix(players: Array[Player]): DenseMatrix[Double] = DenseMatrix.tabulate(players.size, players.size) { case (rowIndex, colIndex) => covariance(players(rowIndex), players(colIndex)) } + DenseMatrix.eye[Double](players.size) * 1e-5
  def covarianceMatrix(playersA: Array[Player], playersB: Array[Player]): DenseMatrix[Double] = DenseMatrix.tabulate(playersA.size, playersB.size) { case (rowIndex, colIndex) => covariance(playersA(rowIndex), playersB(colIndex)) }

}