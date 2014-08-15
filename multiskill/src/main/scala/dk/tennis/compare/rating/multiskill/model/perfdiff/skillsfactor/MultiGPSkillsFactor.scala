package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.GPSkillMath
import org.apache.commons.lang.time.StopWatch

/**
 * Creates gaussian process for every tennis player.
 *
 * @param logSf - day of match covariance -log of signal standard deviation
 * @param logEll - day of match covariance - log of length scale standard deviation
 * @param meanFunc f(player) = prior skill mean for a player
 */
case class MultiGPSkillsFactor(logSf: Double, logEll: Double, meanFunc: Player => Double, players: Array[Player]) extends SkillsFactor {

  private val DAY_MILLIS = 1000L * 3600 * 24

  require(players.size == players.distinct.size, "Players are not unique")

  //key - playerName
  private val playersMap: Map[String, Array[Player]] = toPlayersMap(players)

  private val priorSkillsByPlayersMap: Map[String, MultivariateGaussian] = toPriorSkillsByPlayersMap(players)

  def getPriorSkillsByPlayersMap(): Map[String, MultivariateGaussian] = priorSkillsByPlayersMap

  def getPlayerSkillsPriorMean(): Matrix = Matrix(players.map(p => meanFunc(p)))

  def getPlayerSkillsMarginalMean(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Matrix = {

    //key - playerName
    val marginalSkillsByPlayerMap: Map[String, MultivariateGaussian] = calcMarginalSkillsByPlayerMap(gameSkillsVarUpMsgs)

    val skillsMarginalMeans = players.map { player =>

      val playerIndex = playersMap(player.playerName).indexOf(player)

      val skillMarginal = marginalSkillsByPlayerMap(player.playerName).marginal(playerIndex)
      skillMarginal.m
    }

    Matrix(skillsMarginalMeans)
  }

  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {

    //key - playerName
    val marginalSkillsByPlayerMap: Map[String, MultivariateGaussian] = calcMarginalSkillsByPlayerMap(gameSkillsVarUpMsgs)

    val gameSkillsMarginals = getGameSkillsMarginals(gameSkillsVarUpMsgs, marginalSkillsByPlayerMap)
    gameSkillsMarginals
  }

  def getGameSkillsMarginalsWithD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Tuple2[Seq[CanonicalGaussian], Seq[Seq[MultivariateGaussian]]] = {
    //key - playerName
    val marginalSkillsByPlayerMap: Map[String, MultivariateGaussian] = calcMarginalSkillsByPlayerMap(gameSkillsVarUpMsgs)

    val gameSkillsMarginals = getGameSkillsMarginals(gameSkillsVarUpMsgs, marginalSkillsByPlayerMap)

    val gameSkillsMarginalsD = getGameSkillsMarginalsD(gameSkillsVarUpMsgs, marginalSkillsByPlayerMap)

    (gameSkillsMarginals, gameSkillsMarginalsD)

  }

  /**
   * Returns skills marginals for all games.
   *
   * @param gameSkillsVarUpMsgs Messages send from game skills variables to skills variable for all players
   */
  private def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian], marginalSkillsByPlayerMap: Map[String, MultivariateGaussian]): Seq[CanonicalGaussian] = {

    val gameSkillsMarginals = (0 until gameSkillsVarUpMsgs.size).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val player1Index = playersMap(player1.playerName).indexOf(player1)
      val player2Index = playersMap(player2.playerName).indexOf(player2)

      val skillMarginal1 = marginalSkillsByPlayerMap(player1.playerName).marginal(player1Index)
      val skillMarginal2 = marginalSkillsByPlayerMap(player2.playerName).marginal(player2Index)

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
  private def getGameSkillsMarginalsD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian], marginalSkillsByPlayerMap: Map[String, MultivariateGaussian]): Seq[Seq[MultivariateGaussian]] = {

    //key - playerName
    val marginalSkillsDByPlayerMap: Map[String, Seq[MultivariateGaussian]] = calcMarginalSkillsDByPlayerMap(marginalSkillsByPlayerMap)

    val gamesSkillsMarginalsD = (0 until gameSkillsVarUpMsgs.size).map { gameIndex =>

      val player1 = players(2 * gameIndex)
      val player2 = players(2 * gameIndex + 1)

      val player1Index = playersMap(player1.playerName).indexOf(player1)
      val player2Index = playersMap(player2.playerName).indexOf(player2)

      val p1SkillMarginalsD = marginalSkillsDByPlayerMap(player1.playerName)
      val p2SkillMarginalsD = marginalSkillsDByPlayerMap(player2.playerName)

      val gameSkillsMarginalsD = p1SkillMarginalsD.zip(p2SkillMarginalsD).map {
        case (p1SkillMarginalsD0, p2SkillMarginalsD0) =>

          val p1SkillMarginalD = p1SkillMarginalsD0.marginal(player1Index)
          val p2SkillMarginalD = p2SkillMarginalsD0.marginal(player2Index)

          val m = Matrix(p1SkillMarginalD.m, p2SkillMarginalD.m)
          val v = Matrix(2, 2, Array(p1SkillMarginalD.v, 0, 0, p2SkillMarginalD.v))

          MultivariateGaussian(m, v)
      }

      gameSkillsMarginalsD

    }

    gamesSkillsMarginalsD

  }

  private def toPlayersMap(players: Array[Player]): Map[String, Array[Player]] = {

    val playersMap = players.groupBy(p => p.playerName).mapValues(players => players.sortBy(p => (p.timestamp, p.onServe)))
    playersMap
  }

  /**
   * Returns Map[player name,skillsVarUpMsgs]
   */
  private def toSkillVarUpMsgsByPlayer(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Map[String, Array[CanonicalGaussian]] = {
    val skillsVarUpMsgsByPlayer: Map[String, Array[CanonicalGaussian]] = gameSkillsVarUpMsgs.zipWithIndex.flatMap {
      case (msg, index) =>

        val player1 = players(2 * index)
        val player2 = players(2 * index + 1)

        val player1Index = playersMap(player1.playerName).indexOf(player1)
        val player2Index = playersMap(player2.playerName).indexOf(player2)

        val player1Prior = priorSkillsByPlayersMap(player1.playerName).marginal(player1Index).toCanonical
        val player2Prior = priorSkillsByPlayersMap(player2.playerName).marginal(player2Index).toCanonical

        val lg = CanonicalGaussian(Matrix(2, 2, Array(1d, 0, 0, 1)), Matrix(0, 0), Matrix(2, 2, Array(1e-10, 1e-11, 1e-11, 1e-10)))
        val join = if(!msg.g.isNaN) (msg.extend(4, 2) * lg).marginalise(3).marginalise(2) else lg.marginalise(3).marginalise(2)
        val msgUpPlayer1 = (join * player2Prior.extend(2, 1)).marginalise(1)
        val msgUpPlayer2 = (join * player1Prior.extend(2, 0)).marginalise(0)
        
        Array((msgUpPlayer1, player1), (msgUpPlayer2, player2))
    }.groupBy { case (msg, player) => player.playerName }.mapValues { case msgs => msgs.sortBy(m => (m._2.timestamp, m._2.onServe)).map(m => m._1).toArray }

    skillsVarUpMsgsByPlayer
  }

  def calcMarginalSkillsByPlayerMap(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Map[String, MultivariateGaussian] = {

    //key - player name
    val skillVarUpMsgsByPlayer: Map[String, Array[CanonicalGaussian]] = toSkillVarUpMsgsByPlayer(gameSkillsVarUpMsgs)

    val marginalSkillsByPlayerMap: Map[String, MultivariateGaussian] = playersMap.par.map {
      case (playerName, players) =>

        //prior
        val priorPlayerSkills = priorSkillsByPlayersMap(playerName)

        //likelihood
        val skillVarUpMsgs = skillVarUpMsgsByPlayer(playerName)

        //prior * likelihood
        val playerskillsMarginal = CanonicalGaussian(priorPlayerSkills.m, priorPlayerSkills.v)
        skillVarUpMsgs.zipWithIndex.foreach {
          case (msg, index) =>
            GPSkillMath.updateOnPlayerMsg(playerskillsMarginal, msg, index, true)
        }
       
        (playerName, MultivariateGaussian(playerskillsMarginal.mean, playerskillsMarginal.variance))
    }.toList.toMap

    marginalSkillsByPlayerMap
  }

  private def calcMarginalSkillsDByPlayerMap(marginalSkillsByPlayerMap: Map[String, MultivariateGaussian]): Map[String, Seq[MultivariateGaussian]] = {

    val marginalSkillsDByPlayerMap: Map[String, Seq[MultivariateGaussian]] = playersMap.map {
      case (playerName, players) =>

        val skillsPrior = priorSkillsByPlayersMap(playerName)
        val skillsMarginal = marginalSkillsByPlayerMap(playerName)

        val Kinv = skillsPrior.v.inv

        def calcSkillMarginalD(covD: Matrix): MultivariateGaussian = {
          val skillsMarginalVarD = skillsMarginal.v * Kinv * covD * Kinv * skillsMarginal.v
          val h_d = (-1 * Kinv * covD * Kinv) * skillsPrior.m
          val skillsMarginalMeanD = skillsMarginalVarD * (skillsMarginal.v.inv * skillsMarginal.m) + skillsMarginal.v * h_d
          MultivariateGaussian(skillsMarginalMeanD, skillsMarginalVarD)
        }

        val skillMarginalD_sf = calcSkillMarginalD(covarianceMatrix_df_sf(players, logEll))
        val skillMarginalD_ell = calcSkillMarginalD(covarianceMatrix_df_ell(players, logEll))

        (playerName, List(skillMarginalD_sf, skillMarginalD_ell))
    }

    marginalSkillsDByPlayerMap
  }

  private def toPriorSkillsByPlayersMap(players: Array[Player]): Map[String, MultivariateGaussian] = {

    val priorSkillsByPlayersMap = playersMap.map {
      case (playerName, players) =>

        val priorSkillsMean = Matrix(players.map(p => meanFunc(p)))

        //add some noise on diagonal for numerical stability
        val priorSkillsCov = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(logEll)(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-1

        val priorPlayerSkills = MultivariateGaussian(priorSkillsMean, priorSkillsCov)
        (playerName, priorPlayerSkills)

    }.toMap
    priorSkillsByPlayersMap
  }

  private def covariance(logEll: Double)(player1: Player, player2: Player): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0
    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = logSf, logEll).cov(Matrix(player1.timestamp.getTime / DAY_MILLIS), Matrix(player2.timestamp.getTime / DAY_MILLIS))
    onServeCov * theSamePlayerCov * timeDiffCov

  }

  private def covarianceMatrix_df_sf(players: Array[Player], logSf: Double): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_sf(players(rowIndex), players(colIndex), logSf))
  }

  private def covariance_df_sf(player1: Player, player2: Player, logSf: Double): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = logSf, logEll).df_dSf(Matrix(player1.timestamp.getTime / DAY_MILLIS), Matrix(player2.timestamp.getTime / DAY_MILLIS))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

  private def covarianceMatrix_df_ell(players: Array[Player], logEll: Double): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), logEll))
  }

  private def covariance_df_ell(player1: Player, player2: Player, logEll: Double): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = logSf, logEll).df_dEll(Matrix(player1.timestamp.getTime / DAY_MILLIS), Matrix(player2.timestamp.getTime / DAY_MILLIS))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

}