package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerKey
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkillsFactor

case class AllSkills(players: Seq[Player], priorSkillsByPlayersMap: Map[PlayerKey, PlayerSkillsFactor], skillsMap: Map[PlayerKey, PlayerSkills]) {

  implicit def toPlayerKey(player: Player): PlayerKey = PlayerKey(player.playerName, player.onServe)

  def getPlayerSkillsPriorMean(): DenseVector[Double] = DenseVector(players.toArray.map(p => priorSkillsByPlayersMap(p).priorPlayerSkills.marginal(p).m))

  def getPlayerSkillsMarginalMean(): DenseVector[Double] = {

    val skillsMarginalMeans = players.map { player =>
      skillsMap(player).marginal(player).m
    }.toArray
    DenseVector(skillsMarginalMeans)
  }

  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  def getGameSkillsMarginals(): Seq[DenseCanonicalGaussian] = {
    val gameSkillsMarginals = (0 until players.size / 2).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val skillMarginal1 = skillsMap(player1).marginal(player1)

      val skillMarginal2 = skillsMap(player2).marginal(player2)

      val m = DenseVector(skillMarginal1.m, skillMarginal2.m)
      val v = new DenseMatrix(2, 2, Array(skillMarginal1.v, 0, 0, skillMarginal2.v)).t

      DenseCanonicalGaussian(m, v)
    }

    gameSkillsMarginals

  }

  def getGameSkillsMarginalsWithD(): Tuple2[Seq[DenseCanonicalGaussian], Seq[Seq[MultivariateGaussian]]] = {

    val gameSkillsMarginals = getGameSkillsMarginals()
    val gameSkillsMarginalsD = getGameSkillsMarginalsD()

    (gameSkillsMarginals, gameSkillsMarginalsD)

  }

  /**
   * Returns partial derivatives of skills marginals for all games with respect log length scale of player's covariance.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  private def getGameSkillsMarginalsD(): Seq[Seq[MultivariateGaussian]] = {

    //key - playerName
    val posteriorSkillsDByPlayerMap: Map[PlayerKey, Seq[PlayerSkills]] = calcPosteriorSkillsDByPlayerMap()

    val gamesSkillsMarginalsD = (0 until players.size / 2).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val p1SkillMarginalsD = posteriorSkillsDByPlayerMap(player1)
      val p2SkillMarginalsD = posteriorSkillsDByPlayerMap(player2)

      val gameSkillsMarginalsD = p1SkillMarginalsD.zip(p2SkillMarginalsD).map {
        case (p1SkillMarginalsD0, p2SkillMarginalsD0) =>

          val p1SkillMarginalD = p1SkillMarginalsD0.marginal(player1)
          val p2SkillMarginalD = p2SkillMarginalsD0.marginal(player2)

          val m = DenseVector(p1SkillMarginalD.m, p2SkillMarginalD.m)
          val v = new DenseMatrix(2, 2, Array(p1SkillMarginalD.v, 0, 0, p2SkillMarginalD.v)).t

          MultivariateGaussian(m, v)
      }

      gameSkillsMarginalsD

    }

    gamesSkillsMarginalsD

  }

  private def calcPosteriorSkillsDByPlayerMap(): Map[PlayerKey, Seq[PlayerSkills]] = {

    val marginalSkillsDByPlayerMap: Map[PlayerKey, Seq[PlayerSkills]] = priorSkillsByPlayersMap.map {
      case (playerKey, priorSkills) =>

        val skillsPosterior = skillsMap(playerKey)

        val skillMarginalD = priorSkills.skillPosteriorD(skillsPosterior.skillsGaussian)
        (playerKey, skillMarginalD)
    }

    marginalSkillsDByPlayerMap
  }

  def getPosteriorSkillsForPlayer(playerName: String, skillOnServe: Boolean): Option[PlayerSkills] = {
    skillsMap.get(PlayerKey(playerName, skillOnServe))
  }

}
