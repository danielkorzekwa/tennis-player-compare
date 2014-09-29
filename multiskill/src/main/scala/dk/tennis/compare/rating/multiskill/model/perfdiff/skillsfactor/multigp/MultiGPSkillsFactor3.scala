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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.calcPosteriorSkillsByPlayerMap
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.calcPosteriorSkillsByPlayerMap
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.CovFunc

object MultiGPSkillsFactor3 {
  case class PlayerKey(playerName: String, onServe: Boolean)

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def apply(meanFunc: Player => Double, playerCovFunc: CovFunc, players: Array[Player]): MultiGPSkillsFactor3 = {

    require(players.size == players.distinct.size, "Players are not unique")

    val playersMap = players.groupBy(p => toPlayerKey(p)).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))

    val priorSkillsByPlayersMap = playersMap.map {
      case (playerName, players) =>
        (playerName, PlayerSkillsFactor(meanFunc, playerCovFunc, players))

    }.toMap

    new MultiGPSkillsFactor3(priorSkillsByPlayersMap, players)
  }
}

case class MultiGPSkillsFactor3(priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor], players: Array[Player]) extends SkillsFactor {

  def getPriorSkillsByPlayersMap(): Map[PlayerKey, PlayerSkillsFactor] = priorSkillsByPlayersMap
  def getPlayerSkillsPriorMean(): Matrix = Matrix(players.map(p => priorSkillsByPlayersMap(p).priorPlayerSkills.marginal(p).m))

  def getPlayerSkillsMarginalMean(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Matrix = {

    //key - playerName
    val posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = calcPosteriorSkillsByPlayerMap(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)

    val skillsMarginalMeans = players.map { player =>
      posteriorSkillsByPlayerMap(player).marginal(player).m
    }
    Matrix(skillsMarginalMeans)
  }

  def sampleGameSkills(rand: Random): Seq[MultivariateGaussian] = {

    val sampledSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = priorSkillsByPlayersMap.toMap.map {
      case (playerKey, factor) =>
        val sampledSkillsMean = Matrix(factor.priorPlayerSkills.skillsGaussian.draw())

        val newPlayerSkillFactor = factor.priorPlayerSkills.copy(skillsGaussian = MultivariateGaussian(sampledSkillsMean, factor.priorPlayerSkills.skillsGaussian.v))

        (playerKey, newPlayerSkillFactor)
    }
    val gamesNum = players.size / 2
    val samplesGameSkills = getGameSkillsMarginals(gamesNum, sampledSkillsByPlayerMap).map(c => MultivariateGaussian(c.mean, c.variance))
    samplesGameSkills
  }
  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {

    //key - playerName
    val posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = calcPosteriorSkillsByPlayerMap(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)

    val gameSkillsMarginals = getGameSkillsMarginals(gameSkillsVarUpMsgs.size, posteriorSkillsByPlayerMap)
    gameSkillsMarginals
  }

  def getGameSkillsMarginalsWithD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Tuple2[Seq[CanonicalGaussian], Seq[Seq[MultivariateGaussian]]] = {
    //key - playerName
    val posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = calcPosteriorSkillsByPlayerMap(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)

    val gameSkillsMarginals = getGameSkillsMarginals(gameSkillsVarUpMsgs.size, posteriorSkillsByPlayerMap)
    val gameSkillsMarginalsD = getGameSkillsMarginalsD(gameSkillsVarUpMsgs, posteriorSkillsByPlayerMap)

    (gameSkillsMarginals, gameSkillsMarginalsD)

  }

  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  private def getGameSkillsMarginals(gameNum: Int, posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills]): Seq[CanonicalGaussian] = {

    val gameSkillsMarginals = (0 until gameNum).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val skillMarginal1 = posteriorSkillsByPlayerMap(player1).marginal(player1)

      val skillMarginal2 = posteriorSkillsByPlayerMap(player2).marginal(player2)

      val m = Matrix(skillMarginal1.m, skillMarginal2.m)
      val v = Matrix(2, 2, Array(skillMarginal1.v, 0, 0, skillMarginal2.v))

      CanonicalGaussian(m, v)
    }

    gameSkillsMarginals

  }

  /**
   * Returns partial derivatives of skills marginals for all games with respect log length scale of player's covariance.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  private def getGameSkillsMarginalsD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian], posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills]): Seq[Seq[MultivariateGaussian]] = {

    //key - playerName
    val posteriorSkillsDByPlayerMap: Map[PlayerKey, Seq[PlayerSkills]] = calcPosteriorSkillsDByPlayerMap(posteriorSkillsByPlayerMap)

    val gamesSkillsMarginalsD = (0 until gameSkillsVarUpMsgs.size).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val p1SkillMarginalsD = posteriorSkillsDByPlayerMap(player1)
      val p2SkillMarginalsD = posteriorSkillsDByPlayerMap(player2)

      val gameSkillsMarginalsD = p1SkillMarginalsD.zip(p2SkillMarginalsD).map {
        case (p1SkillMarginalsD0, p2SkillMarginalsD0) =>

          val p1SkillMarginalD = p1SkillMarginalsD0.marginal(player1)
          val p2SkillMarginalD = p2SkillMarginalsD0.marginal(player2)

          val m = Matrix(p1SkillMarginalD.m, p2SkillMarginalD.m)
          val v = Matrix(2, 2, Array(p1SkillMarginalD.v, 0, 0, p2SkillMarginalD.v))

          MultivariateGaussian(m, v)
      }

      gameSkillsMarginalsD

    }

    gamesSkillsMarginalsD

  }

   def getPriorSkillsForPlayer(playerName:String, skillOnServe:Boolean):MultivariateGaussian = {
     priorSkillsByPlayersMap(PlayerKey(playerName, skillOnServe)).priorPlayerSkills.skillsGaussian
   }
  
  def calcPosteriorSkillsForPlayer(playerName: String, skillOnServe: Boolean, gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): MultivariateGaussian = {
    calcPosteriorSkillsByPlayerMap(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)(PlayerKey(playerName, skillOnServe)).skillsGaussian
  }

  private def calcPosteriorSkillsDByPlayerMap(posteriorSkillsByPlayerMap: Map[PlayerKey, PlayerSkills]): Map[PlayerKey, Seq[PlayerSkills]] = {

    val marginalSkillsDByPlayerMap: Map[PlayerKey, Seq[PlayerSkills]] = priorSkillsByPlayersMap.map {
      case (playerKey, priorSkills) =>

        val skillsPosterior = posteriorSkillsByPlayerMap(playerKey)

        val skillMarginalD = priorSkills.skillPosteriorD(skillsPosterior.skillsGaussian)
        (playerKey, skillMarginalD)
    }

    marginalSkillsDByPlayerMap
  }

}