package dk.tennis.compare.rating.multiskill.model.priorskills

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.gpskill.Player

/**
 * Computes prior skills for all players at all tennis games.
 */
object PriorSkills {

//  def priorSkills(players: Array[Player], initialSkillOnServe: Gaussian, initialSkillOnReturn: Gaussian): MultivariateGaussian = {
//
//    val priorSkillsMean = meanVector(players, initialSkillOnServe.m, initialSkillOnReturn.m)
//    val priorSkillsCov = covarianceMatrix(players, players, initialSkillOnServe.v, initialSkillOnReturn.v)
//    val priorSkills = MultivariateGaussian(priorSkillsMean, priorSkillsCov)
//
//    priorSkills
//  }
//
//  def meanVector(players: Array[Player], priorMeanOnServe: Double, priorMeanOnReturn: Double): Matrix = {
//    val data = players.map { p =>
//      if (p.skillOnServe) priorMeanOnServe
//      else priorMeanOnReturn
//    }
//
//    Matrix(data)
//  }
//
//  def covarianceMatrix(playerRows: Array[Player], playerColumns: Array[Player], priorVarOnServe: Double, priorVarOnReturn: Double): Matrix = {
//
//    Matrix(playerRows.size, playerColumns.size, (rowIndex, colIndex) => covariance(playerRows(rowIndex), playerColumns(colIndex), priorVarOnServe, priorVarOnReturn))
//  }
//
  def covariance(player1: Player, player2: Player, priorVarOnServe: Double, priorVarOnReturn: Double): Double = {

    val theSameGameVar = if (player1.equals(player2)) 0.0002 else 0.0001

    val playerOverTimeVar = if (player1.playerName.equals(player2.playerName) && player1.onServe == player2.onServe) {

      val sf = if (player1.onServe) priorVarOnServe
      else priorVarOnReturn
      val l = 0.001
      val skillTimeDiffInDays = (player1.timestamp.getTime() - player2.timestamp.getTime()) / (1000l * 3600 * 24)
      val cov = sf //* exp((-1d / 2 * l * l) * pow(skillTimeDiffInDays, 2))
      cov
    } else 0

    val theSameSkillTypeVar = if (player1.onServe == player2.onServe) 0.1 else 0.0000001

    theSameGameVar + playerOverTimeVar + theSameSkillTypeVar
    
    
  }

}