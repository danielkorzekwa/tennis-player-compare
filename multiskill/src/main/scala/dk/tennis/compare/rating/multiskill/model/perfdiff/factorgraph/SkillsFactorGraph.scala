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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor

case class SkillsFactorGraph(scores: Array[Score], logPerfStdDev: Double, skillsFactor: SkillsFactor) extends Logging {

  val variance = Matrix(2, 2, Array(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity))
  val priorGameToSkillsFactorMsg = CanonicalGaussian(Matrix.zeros(2, 1), variance)

  var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = (1 to scores.size).map { gameFactor =>
    priorGameToSkillsFactorMsg
  }

  def sendMsgs() {

    val gameSkillsMarginals = skillsFactor.getGameSkillsMarginals(gameToSkillsFactorMsgs)
    val skillsToGameMsgs = calcSkillsToGameMsgs(gameSkillsMarginals)

    gameToSkillsFactorMsgs = skillsToGameMsgs.zipWithIndex.map {
      case (skillsToGameMsg, index) =>

        val model = GenericMultiPointCorModel(exp(2 * logPerfStdDev), exp(2 * logPerfStdDev))

        val p1PointsWon = scores(index).p1PointsWon
        val p2PointsWon = scores(index).p2PointsWon

        val newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)
        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        directSkillsMsg

    }

  }

  def calcSkillsToGameMsgs(gameSkillsMarginals: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {
    val skillsToGameMsgs = gameSkillsMarginals.zip(gameToSkillsFactorMsgs).map {
      case (gameSkillsMarginal, gameToSkillsMsg) =>

        val skillsToGameMsg = if (!gameToSkillsMsg.variance(0).isInfinity) gameSkillsMarginal / gameToSkillsMsg else gameSkillsMarginal
        skillsToGameMsg
    }

    skillsToGameMsgs
  }

  def getPlayerSkillsPriorMean(): Matrix = skillsFactor.getPlayerSkillsPriorMean()
  def getPlayerSkillsMarginalMean(): Matrix = skillsFactor.getPlayerSkillsMarginalMean(gameToSkillsFactorMsgs)

}
