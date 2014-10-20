package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc

object calcPosteriorSkillsByPlayerMap {

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def apply(players: Seq[Player], gameSkillsVarUpMsgs: Seq[CanonicalGaussian],
    meanFunc: Player => Double, playerCovFunc: CovFunc): AllSkills = {

    require(players.size == players.distinct.size, "Players are not unique")

    val playersMap = players.groupBy(p => toPlayerKey(p)).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))

    val priorSkillsByPlayersMap = playersMap.map {
      case (playerName, players) =>
        (playerName, PlayerSkillsFactor(meanFunc, playerCovFunc, players.toArray))

    }.toMap

    //key - player name
    val skillVarUpMsgsByPlayer: Map[PlayerKey, Array[CanonicalGaussian]] = toSkillVarUpMsgsByPlayer(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)

    val marginalSkillsByPlayerMap: Map[PlayerKey, PlayerSkills] = priorSkillsByPlayersMap.par.map {
      case (playerKey, priorSkills) =>

        //prior
        val priorPlayerSkills = priorSkillsByPlayersMap(playerKey)

        //likelihood
        val skillVarUpMsgs = skillVarUpMsgsByPlayer(playerKey)

        //prior * likelihood
        val playerSkillsMarginal = priorPlayerSkills.skillPosterior(skillVarUpMsgs)

        (playerKey, PlayerSkills(MultivariateGaussian(playerSkillsMarginal.mean, playerSkillsMarginal.variance), priorSkills.players))
    }.toList.toMap

    AllSkills(players, priorSkillsByPlayersMap, marginalSkillsByPlayerMap, meanFunc, playerCovFunc)
  }

  /**
   * Returns Map[player name,skillsVarUpMsgs]
   */
  private def toSkillVarUpMsgsByPlayer(players: Seq[Player], priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor], gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Map[PlayerKey, Array[CanonicalGaussian]] = {
    val skillsVarUpMsgsByPlayer: Map[PlayerKey, Array[CanonicalGaussian]] = gameSkillsVarUpMsgs.zipWithIndex.flatMap {
      case (msg, index) =>

        val player1 = players(2 * index)
        val player2 = players(2 * index + 1)

        val player1Prior = priorSkillsByPlayersMap(player1).priorPlayerSkills.marginal(player1).toCanonical
        val player2Prior = priorSkillsByPlayersMap(player2).priorPlayerSkills.marginal(player2).toCanonical

        val lg = CanonicalGaussian(Matrix(2, 2, Array(1d, 0, 0, 1)), Matrix(0, 0), Matrix(2, 2, Array(1e-10, 1e-11, 1e-11, 1e-10)))
        val join = if (!msg.g.isNaN) (msg.extend(4, 2) * lg).marginalise(3).marginalise(2) else lg.marginalise(3).marginalise(2)
        val msgUpPlayer1 = (join * player2Prior.extend(2, 1)).marginalise(1)
        val msgUpPlayer2 = (join * player1Prior.extend(2, 0)).marginalise(0)

        Array((msgUpPlayer1, player1), (msgUpPlayer2, player2))
    }.groupBy { case (msg, player) => toPlayerKey(player) }.mapValues { case msgs => msgs.sortBy(m => (m._2.timestamp, m._2.onServe)).map(m => m._1).toArray }

    skillsVarUpMsgsByPlayer
  }
}