package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import java.io.ObjectOutputStream
import java.io.FileOutputStream

/**
 * Inspired by
 *  'Kohei Hayashi, Takashi Takenouchi . Self-measuring Similarity for Multi-task Gaussian Process. 2012'
 *  http://jmlr.org/proceedings/papers/v27/hayashi12a/hayashi12a.pdf
 *
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
class OpponentCovFunc(val params: Seq[Double],
  val skillsOnServeGivenOpponent: Map[String, Seq[Double]], val skillsOnReturnGivenOpponent: Map[String, Seq[Double]]) extends CovFunc with Serializable {
  private val Seq(opponentCovLogSf, opponentCovLogEll) = params

  private val opponentCovFunc = new CovSEiso(sf = opponentCovLogSf, opponentCovLogEll)

  private val opponentOnReturnSimMap = OpponentSimMap("onReturn", (playerName) => skillsOnServeGivenOpponent(playerName).toArray, opponentCovFunc)
  private val opponentOnServeSimMap = OpponentSimMap("onServe", (playerName) => skillsOnReturnGivenOpponent(playerName).toArray, opponentCovFunc)

  def save(file: String) = throw new UnsupportedOperationException("Not implemented yet")

  def withParams(newParams: Seq[Double]): OpponentCovFunc = {
    new OpponentCovFunc(newParams, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
  }

  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  def getParams(): Seq[Double] = params

  def covariance(player1: Player, player2: Player): Double = {

    require(player1.onServe == player2.onServe)

    val covValue = if (player1.onServe) {
      opponentOnReturnSimMap.getCovValue(player1.opponentName, player2.opponentName)
    } else {
      opponentOnServeSimMap.getCovValue(player1.opponentName, player2.opponentName)
    }

    covValue.cov
  }

  def covarianceD(player1: Player, player2: Player, paramIndex: Int): Double = {

    require(player1.onServe == player2.onServe)

    val covValue = if (player1.onServe) {
      opponentOnReturnSimMap.getCovValue(player1.opponentName, player2.opponentName)
    } else {
      opponentOnServeSimMap.getCovValue(player1.opponentName, player2.opponentName)
    }

    val covD = paramIndex match {

      case 0 => covValue.covDSf
      case 1 => covValue.covDEll

    }

    covD
  }

  def opponentOnReturnSimMatrix(players: Seq[String]) = {

    def simMatrixValue(rowIndex: Int, colIndex: Int): Double = {
      val covValue = opponentOnReturnSimMap.getCovValue(players(rowIndex), players(colIndex)).cov

      covValue
    }

    val simMatrix = Matrix(players.size, players.size, (rowIndex, colIndex) => simMatrixValue(rowIndex, colIndex))
    simMatrix
  }

  def opponentOnServeSimMatrix(players: Seq[String]) = {

    def simMatrixValue(rowIndex: Int, colIndex: Int): Double = {
      val covValue = opponentOnServeSimMap.getCovValue(players(rowIndex), players(colIndex)).cov

      covValue
    }

    val simMatrix = Matrix(players.size, players.size, (rowIndex, colIndex) => simMatrixValue(rowIndex, colIndex))
    simMatrix
  }

}

object OpponentCovFunc {

  def apply(params: Seq[Double],
    skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): OpponentCovFunc = {

    new OpponentCovFunc(params,
      skillsOnServeGivenOpponent.mapValues(s => s.map(s => s.skill)).map(identity),
      skillsOnReturnGivenOpponent.mapValues(s => s.map(s => s.skill)).map(identity))
  }
}