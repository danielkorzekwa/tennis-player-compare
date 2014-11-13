package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import scala.util.Random

object sampleGameSkills {

  def apply(players: Seq[Player], meanFunc: Player => Double, playerCovFunc: CovFunc,rand:Random): Seq[MultivariateGaussian] = {

    require(players.size == players.distinct.size, "Players are not unique")

    val playersMap = players.groupBy(p => toPlayerKey(p)).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))

    val priorSkillsByPlayersMap = playersMap.map {
      case (playerName, players) =>
        (playerName, PlayerSkillsFactor(meanFunc, playerCovFunc, players.toArray))

    }.toMap

    val sampledSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = priorSkillsByPlayersMap.toMap.map {
      case (playerKey, factor) =>
        val sampledSkillsMean = Matrix(factor.priorPlayerSkills.skillsGaussian.draw(rand.nextInt))

        val newPlayerSkillFactor = factor.priorPlayerSkills.copy(skillsGaussian = MultivariateGaussian(sampledSkillsMean, factor.priorPlayerSkills.skillsGaussian.v))

        (playerKey, newPlayerSkillFactor)
    }

    val sampledGameSkills = AllSkills(players, priorSkillsByPlayersMap, sampledSkillsByPlayerMap).getGameSkillsMarginals().map(c => MultivariateGaussian(c.mean, c.variance))
    sampledGameSkills

  }

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)
}