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
import dk.bayes.math.gaussian.canonical.CanonicalGaussian

object calcPosteriorSkillsByPlayerMap {

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def apply(players: Seq[Player], gameSkillsVarUpMsgs: Seq[DenseCanonicalGaussian], priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor]): AllSkills = {

    //key - player name
    val skillVarUpMsgsByPlayer: Map[PlayerKey, Array[DenseCanonicalGaussian]] = toSkillVarUpMsgsByPlayer(players, gameSkillsVarUpMsgs)

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
  private def toSkillVarUpMsgsByPlayer(players: Seq[Player], gameSkillsVarUpMsgs: Seq[DenseCanonicalGaussian]): Map[PlayerKey, Array[DenseCanonicalGaussian]] = {
    val skillsVarUpMsgsByPlayer: Map[PlayerKey, Array[DenseCanonicalGaussian]] = gameSkillsVarUpMsgs.zipWithIndex.flatMap {
      case (msg, index) =>

        val player1 = players(2 * index)
        val player2 = players(2 * index + 1)

        val msgUpPlayer1 = msg.marginal(0)
         val msgUpPlayer2 = msg.marginal(1)
        
        
        Array((msgUpPlayer1, player1), (msgUpPlayer2, player2))
    }.groupBy { case (msg, player) => toPlayerKey(player) }.mapValues { case msgs => msgs.sortBy(m => (m._2.timestamp, m._2.onServe)).map(m => m._1).toArray }

    skillsVarUpMsgsByPlayer
  }
}