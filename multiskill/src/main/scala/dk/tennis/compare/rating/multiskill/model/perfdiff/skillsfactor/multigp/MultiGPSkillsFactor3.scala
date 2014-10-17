package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp

import scala.Array.canBuildFrom
import scala.util.Random
import MultiGPSkillsFactor3._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.calcPosteriorSkillsByPlayerMap
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.calcPosteriorSkillsByPlayerMap
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc
import dk.tennis.compare.rating.multiskill.infer.skillgivenskills.inferSkillGivenSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.AllSkills

object MultiGPSkillsFactor3 {

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def apply(meanFunc: Player => Double, playerCovFunc: CovFunc, players: Array[Player]): MultiGPSkillsFactor3 = {

    require(players.size == players.distinct.size, "Players are not unique")

    val playersMap = players.groupBy(p => toPlayerKey(p)).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))

    val priorSkillsByPlayersMap = playersMap.map {
      case (playerName, players) =>
        (playerName, PlayerSkillsFactor(meanFunc, playerCovFunc, players))

    }.toMap

    new MultiGPSkillsFactor3(priorSkillsByPlayersMap, players, meanFunc, playerCovFunc)
  }
}

case class MultiGPSkillsFactor3(priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor], players: Array[Player], meanFunc: Player => Double, playerCovFunc: CovFunc) extends SkillsFactor {

  def calcPosteriorSkillsByPlayerMap2(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): AllSkills = {
    val posteriorSkills = calcPosteriorSkillsByPlayerMap(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs, meanFunc, playerCovFunc)
    posteriorSkills
  }

  def sampleGameSkills(rand: Random): Seq[MultivariateGaussian] = {

    val sampledSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = priorSkillsByPlayersMap.toMap.map {
      case (playerKey, factor) =>
        val sampledSkillsMean = Matrix(factor.priorPlayerSkills.skillsGaussian.draw())

        val newPlayerSkillFactor = factor.priorPlayerSkills.copy(skillsGaussian = MultivariateGaussian(sampledSkillsMean, factor.priorPlayerSkills.skillsGaussian.v))

        (playerKey, newPlayerSkillFactor)
    }

    val samplesGameSkills = AllSkills(players, priorSkillsByPlayersMap, sampledSkillsByPlayerMap, meanFunc, playerCovFunc).getGameSkillsMarginals().map(c => MultivariateGaussian(c.mean, c.variance))
    samplesGameSkills
  }
}