package dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops.calcPosteriorSkillsByPlayerMap
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel

case class SkillsFactorGraph(meanFunc: Player => Double, playerCovFunc: CovFunc, scores: Array[Score], logPerfStdDev: Double) extends Logging {

  val variance = Matrix(2, 2, Array(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity))
  val priorGameSkillsVarUpMsg = CanonicalGaussian(Matrix.zeros(2, 1), variance)

  var gameSkillsVarUpMsgs: Seq[CanonicalGaussian] = (1 to scores.size).map { gameFactor =>
    priorGameSkillsVarUpMsg
  }

  val players = Score.toPlayers(scores)

  var allSkills = calcPosteriorSkillsByPlayerMap(players, gameSkillsVarUpMsgs, meanFunc, playerCovFunc)

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
              val newDirectSkillsMean = Matrix(Array(newDirectSkill1.m, newDirectSkill2.m))
              val newDirectSkillsVar = Matrix(2, 2, Array(newDirectSkill1.v, 0d, 0d, newDirectSkill2.v))
              val newDirectSkills = CanonicalGaussian(newDirectSkillsMean, newDirectSkillsVar)

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

    allSkills = calcPosteriorSkillsByPlayerMap(players, gameSkillsVarUpMsgs, meanFunc, playerCovFunc)
  }

  def calcSkillsToGameMsgs(gameSkillsMarginals: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {

    val skillsToGameMsgs = gameSkillsMarginals.zip(gameSkillsVarUpMsgs).map {
      case (gameSkillsMarginal, gameToSkillsMsg) =>

        val skillsToGameMsg = gameSkillsMarginal / gameToSkillsMsg
        skillsToGameMsg
    }

    skillsToGameMsgs
  }

  def getPlayerSkillsPriorMean(): Matrix = allSkills.getPlayerSkillsPriorMean()
  def getPlayerSkillsMarginalMean(): Matrix = allSkills.getPlayerSkillsMarginalMean()

}
