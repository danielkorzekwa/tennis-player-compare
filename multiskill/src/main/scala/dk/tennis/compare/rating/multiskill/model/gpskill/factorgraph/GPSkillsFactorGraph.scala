package dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel

case class GPSkillsFactorGraph(priorPlayerSkills: MultivariateGaussian, players: Array[Player],
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends Logging {

  private val canonPriorSkills = CanonicalGaussian(priorPlayerSkills.m, priorPlayerSkills.v)

  private var skillsMarginal: MultivariateGaussian = priorPlayerSkills

  val precision = Matrix(2, 2, Array.fill(4)(0d))
  val priorGameToSkillsFactorMsg = new CanonicalGaussian(
    precision, precision * Matrix.zeros(2, 1),
    Double.NaN)

  private var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = (1 to players.size / 2).map { gameFactor =>
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

        val p1PointsWon = players(index * 2).pointsWon
        val p2PointsWon = players(index * 2 + 1).pointsWon
        val newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        directSkillsMsg
    }

    //compute marginal

    val copy = new CanonicalGaussian(canonPriorSkills.k.copy,canonPriorSkills.h.copy,canonPriorSkills.g)
    val canonSkillsMarginal = gameToSkillsFactorMsgs.zipWithIndex.foldLeft(copy) {
      case (marginal, (gameToSkillMsg, index)) =>

        val k00 = marginal.k(index * 2, index * 2)
        marginal.k.set(index * 2, index * 2, k00 + gameToSkillMsg.k(0, 0))

        val k01 = marginal.k(index * 2, index * 2 + 1)
        marginal.k.set(index * 2, index * 2 + 1, k01 + gameToSkillMsg.k(0, 1))

        val k10 = marginal.k(index * 2 + 1, index * 2)
        marginal.k.set(index * 2 + 1, index * 2, k10 + gameToSkillMsg.k(1, 0))

        val k11 = marginal.k(index * 2 + 1, index * 2 + 1)
        marginal.k.set(index * 2 + 1, index * 2 + 1, k11 + gameToSkillMsg.k(1, 1))

        val h0 = marginal.h(index * 2)
        marginal.h.set(index * 2, 0, h0 + gameToSkillMsg.h(0))

        val h1 = marginal.h(index * 2 + 1)
        marginal.h.set(index * 2 + 1, 0, h1 + gameToSkillMsg.h(1))

        marginal

    }

    skillsMarginal = MultivariateGaussian(canonSkillsMarginal.mean, canonSkillsMarginal.variance)

  }

  def getPlayerSkillsMarginal(): MultivariateGaussian = skillsMarginal

}
