package dk.tennis.compare.rating.multiskill.model.gpskill.naive.factorgraph

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
import dk.tennis.compare.rating.multiskill.model.gpskill.Score
import dk.tennis.compare.rating.multiskill.model.gpskill.naive.GPSkillMath

case class GPSkillsFactorGraph(priorPlayerSkills: MultivariateGaussian, scores: Array[Score],
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends Logging {

  private val canonPriorSkills = CanonicalGaussian(priorPlayerSkills.m, priorPlayerSkills.v)

  private var skillsMarginal: MultivariateGaussian = priorPlayerSkills

  val precision = Matrix(2, 2, Array.fill(4)(0d))
  val priorGameToSkillsFactorMsg = new CanonicalGaussian(
    precision, precision * Matrix.zeros(2, 1),
    Double.NaN)

  var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = (1 to scores.size ).map { gameFactor =>
    priorGameToSkillsFactorMsg
  }
  def sendMsgs() {
    //compute gameToSkillsFactorMsgs
    gameToSkillsFactorMsgs = gameToSkillsFactorMsgs.zipWithIndex.map {
      case (gameToSkillsMsg, index) =>

        val (mean, variance) = (skillsMarginal.m, skillsMarginal.v)
        val directSkillsMean = Matrix(mean(index * 2), mean(index * 2 + 1))
        val var00 = variance(index * 2, index * 2)
        val var01 = variance(index * 2, index * 2 + 1)
        val var10 = variance(index * 2 + 1, index * 2)
        val var11 = variance(index * 2 + 1, index * 2 + 1)
        val directSkillsVariance = Matrix(2, 2, Array(var00, var01, var10, var11))

        val skillsToGameMsg = CanonicalGaussian(directSkillsMean, directSkillsVariance) / gameToSkillsMsg

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

        val p1PointsWon = scores(index).p1PointsWon
        val p2PointsWon = scores(index).p2PointsWon
        val newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        directSkillsMsg
    }

    //compute marginal

    val canonSkillsMarginal = new CanonicalGaussian(canonPriorSkills.k.copy, canonPriorSkills.h.copy, canonPriorSkills.g)
    gameToSkillsFactorMsgs.zipWithIndex.foreach {
      case (gameToSkillMsg, index) =>
        GPSkillMath.updateProduct(canonSkillsMarginal, gameToSkillMsg, index,true)
    }

    skillsMarginal = MultivariateGaussian(canonSkillsMarginal.mean, canonSkillsMarginal.variance)

  }

  def getPlayerSkillsMarginal(): MultivariateGaussian = skillsMarginal

}
