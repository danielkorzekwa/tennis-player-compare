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

case class SkillsFactorGraph(scores: Array[Score], perfVarianceOnServe: Double, perfVarianceOnReturn: Double, skillsFactor: SkillsFactor) extends Logging {

  val precision = Matrix(2, 2, Array.fill(4)(0d))
  val priorGameToSkillsFactorMsg = new CanonicalGaussian(
    precision, precision * Matrix.zeros(2, 1),
    Double.NaN)

  var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = (1 to scores.size).map { gameFactor =>
    priorGameToSkillsFactorMsg
  }

  def sendMsgs() {

    val gameSkillsMarginals = skillsFactor.getGameSkillsMarginals(gameToSkillsFactorMsgs)
    val skillsToGameMsgs = calcSkillsToGameMsgs()

    gameToSkillsFactorMsgs = skillsToGameMsgs.zipWithIndex.map {
      case (skillsToGameMsg, index) =>

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

        val p1PointsWon = scores(index).p1PointsWon
        val p2PointsWon = scores(index).p2PointsWon

        val newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        directSkillsMsg

    }

  }

  def calcSkillsToGameMsgs(): Seq[CanonicalGaussian] = {
    val gameSkillsMarginals = skillsFactor.getGameSkillsMarginals(gameToSkillsFactorMsgs)

    val skillsToGameMsgs = gameSkillsMarginals.zip(gameToSkillsFactorMsgs).map {
      case (gameSkillsMarginal, gameToSkillsMsg) =>

        val skillsToGameMsg = gameSkillsMarginal / gameToSkillsMsg
        skillsToGameMsg
    }

    skillsToGameMsgs
  }

  def getPlayerSkillsMarginalMean(): Matrix = skillsFactor.getPlayerSkillsMarginalMean(gameToSkillsFactorMsgs)

}
