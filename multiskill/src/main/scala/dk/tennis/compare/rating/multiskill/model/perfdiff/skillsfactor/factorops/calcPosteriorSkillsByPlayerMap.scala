package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

object calcPosteriorSkillsByPlayerMap {

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def apply(players: Seq[Player], gameSkillsVarUpMsgs: Seq[DenseCanonicalGaussian], priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor]): AllSkills = {

    //key - player name
    val skillVarUpMsgsByPlayer: Map[PlayerKey, Array[DenseCanonicalGaussian]] = toSkillVarUpMsgsByPlayer(players, priorSkillsByPlayersMap, gameSkillsVarUpMsgs)

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

    AllSkills(players, priorSkillsByPlayersMap, marginalSkillsByPlayerMap)
  }

  /**
   * Returns Map[player name,skillsVarUpMsgs]
   */
  private def toSkillVarUpMsgsByPlayer(players: Seq[Player], priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor], gameSkillsVarUpMsgs: Seq[DenseCanonicalGaussian]): Map[PlayerKey, Array[DenseCanonicalGaussian]] = {
    val skillsVarUpMsgsByPlayer: Map[PlayerKey, Array[DenseCanonicalGaussian]] = gameSkillsVarUpMsgs.zipWithIndex.flatMap {
      case (msg, index) =>

        val player1 = players(2 * index)
        val player2 = players(2 * index + 1)

        val player1Prior = priorSkillsByPlayersMap(player1).priorPlayerSkills.marginal(player1).toCanonical
        val player2Prior = priorSkillsByPlayersMap(player2).priorPlayerSkills.marginal(player2).toCanonical

        val lg = DenseCanonicalGaussian(new DenseMatrix(2, 2, Array(1d, 0, 0, 1)).t, DenseVector(0.0, 0), new DenseMatrix(2, 2, Array(1e-10, 1e-11, 1e-11, 1e-10)).t)
        val join = if (!msg.g.isNaN) (msg.extend(4, 2) * lg).marginalise(3).marginalise(2) else lg.marginalise(3).marginalise(2)
        val msgUpPlayer1 = (join * player2Prior.extend(2, 1)).marginalise(1)
        val msgUpPlayer2 = (join * player1Prior.extend(2, 0)).marginalise(0)

        Array((msgUpPlayer1, player1), (msgUpPlayer2, player2))
    }.groupBy { case (msg, player) => toPlayerKey(player) }.mapValues { case msgs => msgs.sortBy(m => (m._2.timestamp, m._2.onServe)).map(m => m._1).toArray }

    skillsVarUpMsgsByPlayer
  }
}