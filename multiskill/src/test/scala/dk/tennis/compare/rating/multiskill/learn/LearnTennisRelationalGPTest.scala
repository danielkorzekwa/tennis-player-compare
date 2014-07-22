package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import dk.bayes.math.linear.Matrix
import java.util.Date
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import dk.bayes.math.gaussian.CanonicalGaussian

class LearnTennisRelationalGPTest {

  //  val perfVar = 150
  //
  //  val players1 = Array(
  //    Player("0", "1", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("1", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "2", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("2", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "1", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("1", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "2", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("2", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //
  //    Player("2", "3", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("3", "2", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("1", "3", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("3", "1", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("2", "3", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("3", "2", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("1", "3", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("3", "1", new Date(1), pointsWon = 40, skillOnServe = true),
  //
  //    Player("0", "4", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("4", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "5", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("5", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "6", new Date(1), pointsWon = 60, skillOnServe = true),
  //    Player("6", "0", new Date(1), pointsWon = 40, skillOnServe = true),
  //    Player("0", "7", new Date(1), pointsWon = 50, skillOnServe = true),
  //    Player("7", "0", new Date(1), pointsWon = 50, skillOnServe = true))
  //
  //  val players = (1 to 2).flatMap(i => players1).toArray
  //  val playersNum = players.map(p => p.playerName).distinct.size
  //
  //  println("playersNum=" + players.size)
  //
  //  var player1CovMatrix = Matrix(playersNum, playersNum, (pId1, pId2) => if (pId1 == pId2) 1 else 0.5)
  //  var player2CovMatrix = Matrix(playersNum, playersNum, (pId1, pId2) => if (pId1 == pId2) 1 else 0.5)
  //
  //  @Test def test {
  //
  //    for (i <- 1 to 20) {
  //
  //      println("player1CovMatrix:\n" + player1CovMatrix)
  //      println("player2CovMatrix:\n" + player2CovMatrix)
  //
  //      val currCovFunc = covFunc(player1CovMatrix, player2CovMatrix) _
  //      val gp = GenericGPSkillsInfer(perfVar, perfVar, players, currCovFunc, meanFunc, threshold = 0.6)
  //      println("skill covariance:" + gp.priorSkillsCov)
  //
  //      val loglik = gp.loglik()
  //      println("loglik" + loglik + "\n")
  //
  //      val predictionMatrix = calcSkillsMatrix(gp)
  //      println("predictionMatrix:\n" + predictionMatrix)
  //
  //      val pointProbMatrix = probMatrix(gp)
  //      println("pointProbMatrix:\n" + pointProbMatrix)
  //
  //      player1CovMatrix = calcPlayer1CovMatrix(pointProbMatrix)
  //      player2CovMatrix = calcPlayer2CovMatrix(pointProbMatrix)
  //
  //      println("-----------------------------------")
  //    }
  //
  //  }
  //
  //  private def calcSkillsMatrix(gp: GenericGPSkillsInfer): Matrix = {
  //    def playerSkill(playerId1: Int, playerId2: Int) = {
  //      val player = Player(playerId1.toString, playerId2.toString, new Date(0), pointsWon = 30, skillOnServe = true)
  //      gp.playerSkill(player).m
  //    }
  //    val predictionMatrix = Matrix(playersNum, playersNum, (rowIndex, colIndex) => playerSkill(rowIndex, colIndex))
  //    predictionMatrix
  //  }
  //
  //  private def probMatrix(gp: GenericGPSkillsInfer): Matrix = {
  //    val pointModel = GenericPointCorModel(perfVar, perfVar)
  //
  //    def pointProb(p1Index: Int, p2Index: Int): Double = {
  //      val player1 = Player(p1Index.toString, p2Index.toString, new Date(0), pointsWon = 30, skillOnServe = true)
  //      val player2 = Player(p2Index.toString, p1Index.toString, new Date(0), pointsWon = 30, skillOnServe = true)
  //
  //      val p1Skill = gp.playerSkill(player1)
  //      val p2Skill = gp.playerSkill(player2)
  //      val skillsMean = Matrix(p1Skill.m, p2Skill.m)
  //      val skillsCov = Matrix.diag(p1Skill.v, p2Skill.v)
  //      val skills = CanonicalGaussian(skillsMean, skillsCov)
  //
  //      val pointProb = pointModel.pointProb(skills)
  //
  //      pointProb
  //
  //    }
  //    val probMatrix = Matrix(playersNum, playersNum, (rowIndex, colIndex) => pointProb(rowIndex, colIndex))
  //    probMatrix
  //  }
  //
  //  private def calcPlayer1CovMatrix(predictionMatrix: Matrix): Matrix = {
  //
  //    val covFunc = CovSEiso(log(1), log(0.1))
  //    Matrix(playersNum, playersNum, (rowIndex, colIndex) =>
  //      covFunc.cov(predictionMatrix.row(rowIndex).t, predictionMatrix.row(colIndex).t))
  //  }
  //
  //  private def calcPlayer2CovMatrix(predictionMatrix: Matrix): Matrix = {
  //    val covFunc = CovSEiso(log(1), log(0.1))
  //    Matrix(playersNum, playersNum, (rowIndex, colIndex) =>
  //      covFunc.cov(predictionMatrix.column(rowIndex), predictionMatrix.column(colIndex)))
  //  }
  //
  //  private def covFunc(player1CovMatrix: Matrix, player2CovMatrix: Matrix)(player1: Player, player2: Player): Double = {
  //
  //    val p1Kernel = player1CovMatrix(player1.playerName.toInt, player2.playerName.toInt)
  //    val p2Kernel = player2CovMatrix(player1.opponentName.toInt, player2.opponentName.toInt)
  //
  //    p1Kernel * p2Kernel
  //  }
  //
  //  def meanFunc(player: Player): Double = 0d

}