package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix

trait PlayerCovFunc {

  def covarianceMatrix(players: Array[Player]): Matrix

  def covarianceMatrixD(players: Array[Player]): Seq[Matrix]

}