package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import dk.bayes.infer.gp.cov.CovSEiso
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import java.io.ObjectInputStream
import java.io.FileInputStream
import java.util.Date
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.perfdiff.NumOfSets
import breeze.linalg.DenseMatrix

/**
 * Inspired by
 *  'Kohei Hayashi, Takashi Takenouchi . Self-measuring Similarity for Multi-task Gaussian Process. 2012'
 *  http://jmlr.org/proceedings/papers/v27/hayashi12a/hayashi12a.pdf
 *
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
class OpponentCovFunc(val params: Seq[Double], scores: Seq[Score],
  val skillsOnServeGivenOpponent: Map[String, Seq[Gaussian]], val skillsOnReturnGivenOpponent: Map[String, Seq[Gaussian]]) extends CovFunc with Serializable {
  private val Seq(opponentCovLogSf, opponentCovLogEll) = params

  private val opponentCovFunc = new CovSEiso(sf = opponentCovLogSf, opponentCovLogEll)

  private val opponentOnReturnSimMap = OpponentSimMap("onReturn", (playerName) => skillsOnServeGivenOpponent(playerName).toArray, opponentCovFunc)
  private val opponentOnServeSimMap = OpponentSimMap("onServe", (playerName) => skillsOnReturnGivenOpponent(playerName).toArray, opponentCovFunc)

  def save(file: String) = new ObjectOutputStream(new FileOutputStream(file)).writeObject(this)

  def withParams(newParams: Seq[Double]): OpponentCovFunc = {
    new OpponentCovFunc(newParams, scores, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)
  }

  def withPlayerSkills(getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
    OpponentCovFunc(params, scores, getPlayerSkill)
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

    val simMatrix = DenseMatrix.tabulate(players.size, players.size){case (rowIndex, colIndex) => simMatrixValue(rowIndex, colIndex)}
      
    simMatrix
  }

  def opponentOnServeSimMatrix(players: Seq[String]) = {

    def simMatrixValue(rowIndex: Int, colIndex: Int): Double = {
      val covValue = opponentOnServeSimMap.getCovValue(players(rowIndex), players(colIndex)).cov

      covValue
    }

    val simMatrix = DenseMatrix.tabulate(players.size, players.size){case (rowIndex, colIndex) => simMatrixValue(rowIndex, colIndex)}
    simMatrix
  }

}

object OpponentCovFunc {

  def fromFile(file: String): OpponentCovFunc = {
    new ObjectInputStream(new FileInputStream(file)).readObject().asInstanceOf[OpponentCovFunc]
  }

  def apply(params: Seq[Double], scores: Seq[Score], getPlayerSkill: (Player) => PlayerSkill): OpponentCovFunc = {

    val Seq(
      opponentCovLogSf, opponentCovLogEll) = params

    val allPlayers = scores.map(s => s.player1.playerName).distinct
    val skillsOnServeGivenOpponent = calcPriorSkillsGivenOpponent(allPlayers, getPlayerSkill, onServe = true) map identity
    val skillsOnReturnGivenOpponent = calcPriorSkillsGivenOpponent(allPlayers, getPlayerSkill, onServe = false) map identity

    val opponentCovFunc = OpponentCovFunc(Array(opponentCovLogSf, opponentCovLogEll), scores, skillsOnServeGivenOpponent, skillsOnReturnGivenOpponent)

    opponentCovFunc
  }

  def apply(params: Seq[Double], scores: Seq[Score],
    skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): OpponentCovFunc = {

    println(skillsOnServeGivenOpponent("Roger Federer").take(10))
    println(skillsOnServeGivenOpponent("Andy Murray").take(10))
    println(skillsOnServeGivenOpponent("Omar Awadhy").take(10))
    println(skillsOnServeGivenOpponent("Zhe Li").take(10))
    println(skillsOnServeGivenOpponent("Novak Djokovic").take(10))

    //println(skillsOnReturnGivenOpponent("Adrian Mannarino").take(10))
    //  println(skillsOnReturnGivenOpponent("Albert Ramos").take(10))
    //  println(skillsOnReturnGivenOpponent("p3").take(10))
    //   println(skillsOnReturnGivenOpponent("p10").take(10))

    new OpponentCovFunc(params, scores,
      skillsOnServeGivenOpponent.mapValues(s => s.map(s => s.skill)).map(identity),
      skillsOnReturnGivenOpponent.mapValues(s => s.map(s => s.skill)).map(identity))
  }

  /**
   * Returns Map[opponent name, player skills against opponent]
   */
  private def calcPriorSkillsGivenOpponent(allPlayers: Seq[String], getPlayerSkill: (Player) => PlayerSkill, onServe: Boolean): Map[String, Seq[PlayerSkill]] = {

    val skillsGivenOpponentMap = allPlayers.map { playerKey =>

      val skills = allPlayers.map { p =>
        val player = Player(p, playerKey, onServe, new Date(0),Surface.HARD,NumOfSets.THREE_SETS)
        //  val opponent = Player(playerKey,p, !onServe, new Date(0))
        val skill = getPlayerSkill(player)
        //   val playerSkill = getPlayerSkill(player)
        //    val opponentSkill = getPlayerSkill(opponent)
        //    import scala.math._
        //  skill.copy(skill=abs(playerSkill.skill-opponentSkill.skill))
        skill
      }.toSeq

      (playerKey, skills)
    }.toMap

    skillsGivenOpponentMap
  }
}