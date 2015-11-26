package dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph

import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops.calcPosteriorSkillsByPlayerMap
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import breeze.linalg.Matrix
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

case class SkillsFactorGraph(meanFunc: Player => Double, playerCovFunc: CovFunc, scores: Array[Score], logPerfStdDev: Double) extends LazyLogging {

  private val variance = new DenseMatrix(2, 2, Array(100d,0, 0,100d)).t + DenseMatrix.eye[Double](2)*1e-7

  private val priorGameSkillsVarUpMsg = DenseCanonicalGaussian(DenseVector.zeros[Double](2), variance)
  private var gameSkillsVarUpMsgs: Seq[DenseCanonicalGaussian] = (1 to scores.size).map { gameFactor =>
    priorGameSkillsVarUpMsg
  }

  val players = Score.toPlayers(scores)
  require(players.size == players.distinct.size, "Players are not unique")

  val playersMap = players.groupBy(p => toPlayerKey(p)).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))
  val priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor] = playersMap.map {
    case (playerName, players) =>
      (playerName, PlayerSkillsFactor(meanFunc, playerCovFunc, players.toArray))

  }.toMap

  var allSkills = calcPosteriorSkillsByPlayerMap(players, gameSkillsVarUpMsgs,   priorSkillsByPlayersMap)

  def sendMsgs() {

    val skillsToGameMsgs = calcSkillsToGameMsgs(allSkills.getGameSkillsMarginals())

    gameSkillsVarUpMsgs = skillsToGameMsgs.zip(gameSkillsVarUpMsgs).zipWithIndex.map {
      case ((skillsToGameMsg, gameSkillsVarUpMsg), index) =>

        val newGameSkillsVarUpMsg = scores(index).pointsWon match {
          case Some((p1PointsWon, p2PointsWon)) => {
            try {
              val model = GenericMultiPointCorModel(exp(2 * logPerfStdDev), exp(2 * logPerfStdDev))
              val model2 = GenericMultiPointModel(exp(2 * logPerfStdDev), exp(2 * logPerfStdDev))

              //     var newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)
              //      newDirectSkills = CanonicalGaussian(newDirectSkills.mean, Matrix(2, 2, Array(newDirectSkills.variance(0, 0), 0d, 0d, newDirectSkills.variance(1, 1))))

              val (newDirectSkill1, newDirectSkill2, factorGraph) = model2.skillMarginals(skillsToGameMsg.marginal(0), skillsToGameMsg.marginal(1), p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)
              val newDirectSkillsMean = DenseVector(Array(newDirectSkill1.m, newDirectSkill2.m))
              val newDirectSkillsVar = new DenseMatrix(2, 2, Array(newDirectSkill1.v, 0d, 0d, newDirectSkill2.v)).t
              val newDirectSkills = DenseCanonicalGaussian(newDirectSkillsMean, newDirectSkillsVar)

              var directSkillsMsg = newDirectSkills / skillsToGameMsg
              directSkillsMsg
            } catch {
              case e: Exception => {
                logger.debug("Message passing error (single message)")
                gameSkillsVarUpMsg
              }
            }
          }
          case None => gameSkillsVarUpMsg
        }

        newGameSkillsVarUpMsg

    }

    allSkills = calcPosteriorSkillsByPlayerMap(players, gameSkillsVarUpMsgs,   priorSkillsByPlayersMap)
  }

  def calcSkillsToGameMsgs(gameSkillsMarginals: Seq[DenseCanonicalGaussian]): Seq[DenseCanonicalGaussian] = {

    val skillsToGameMsgs = gameSkillsMarginals.zip(gameSkillsVarUpMsgs).map {
      case (gameSkillsMarginal, gameToSkillsMsg) =>

        val skillsToGameMsg = gameSkillsMarginal / gameToSkillsMsg
        skillsToGameMsg
    }

    skillsToGameMsgs
  }

  def getPlayerSkillsPriorMean(): DenseVector[Double] = allSkills.getPlayerSkillsPriorMean()
  def getPlayerSkillsMarginalMean(): DenseVector[Double] = allSkills.getPlayerSkillsMarginalMean()

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)
}
