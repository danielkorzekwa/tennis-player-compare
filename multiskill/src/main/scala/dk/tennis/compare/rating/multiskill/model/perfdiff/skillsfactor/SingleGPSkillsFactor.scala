package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import scala.Array.canBuildFrom
import dk.tennis.compare.rating.multiskill.model.perfdiff.GPSkillMath

/**
 * Uses a single gp for all players across all games. It's a reference implementation and it does not scale well.
 * - run time O(n^3)
 * - space O(n^2)
 *
 * where n - number of games*2
 *
 * @param priorPlayerSkills
 * @param covD Partial derivatives of players covariance with respect to hyper parameters
 */
case class SingleGPSkillsFactor(ell: Double, players: Array[Player]) extends SkillsFactor {

  private val priorSkillsMean = Matrix(players.map(p => meanFunc(p)))

  //add some noise on diagonal for numerical stability
  private val priorSkillsCov = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(ell)(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-1

  private val priorPlayerSkills = CanonicalGaussian(priorSkillsMean, priorSkillsCov)

  private val covD = covarianceMatrix_df_ell(players, ell)

  def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {

    val skillsMarginal = calcSkillsMarginal(gameSkillsVarUpMsgs)

    val gameSkillsMarginals = gameSkillsVarUpMsgs.zipWithIndex.map {
      case (gameSkillsVarUpMsg, index) =>

        val (mean, variance) = (skillsMarginal.mean, skillsMarginal.variance)
        val directSkillsMean = Matrix(mean(index * 2), mean(index * 2 + 1))
        val var00 = variance(index * 2, index * 2)
        val var01 = variance(index * 2, index * 2 + 1)
        val var10 = variance(index * 2 + 1, index * 2)
        val var11 = variance(index * 2 + 1, index * 2 + 1)
        val directSkillsVariance = Matrix(2, 2, Array(var00, var01, var10, var11))

        val gameSkillsMarginal = CanonicalGaussian(directSkillsMean, directSkillsVariance)

        gameSkillsMarginal
    }

    gameSkillsMarginals
  }

  def getGameSkillsMarginalsD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[MultivariateGaussian] = {

    val Kinv = priorPlayerSkills.variance.inv

    val skillsMarginal = calcSkillsMarginal(gameSkillsVarUpMsgs)

    val skillsMarginalVarD = skillsMarginal.variance * Kinv * covD * Kinv * skillsMarginal.variance
    val h_d = (-1 * Kinv * covD * Kinv) * priorPlayerSkills.mean
    val skillsMarginalMeanD = skillsMarginalVarD * skillsMarginal.h

    val gameSkillsMarginalsD = (0 until gameSkillsVarUpMsgs.size).map { index =>

      val gameSkillsMarginalD = GPSkillMath.getSkills(MultivariateGaussian(skillsMarginalMeanD, skillsMarginalVarD), index)
      gameSkillsMarginalD
    }

    gameSkillsMarginalsD
  }

  def getPlayerSkillsMarginalMean(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Matrix = {
    val skillsMarginal = new CanonicalGaussian(priorPlayerSkills.k.copy, priorPlayerSkills.h.copy, priorPlayerSkills.g)

    gameSkillsVarUpMsgs.zipWithIndex.foreach {
      case (gameToSkillMsg, index) =>
        GPSkillMath.updateProduct(skillsMarginal, gameToSkillMsg, index, true)
    }

    skillsMarginal.mean
  }

  private def calcSkillsMarginal(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): CanonicalGaussian = {
    val skillsMarginal = new CanonicalGaussian(priorPlayerSkills.k.copy, priorPlayerSkills.h.copy, priorPlayerSkills.g)

    gameSkillsVarUpMsgs.zipWithIndex.foreach {
      case (gameToSkillMsg, index) =>
        GPSkillMath.updateProduct(skillsMarginal, gameToSkillMsg, index, true)
    }

    skillsMarginal
  }

  private def covariance(ell: Double)(player1: Player, player2: Player): Double = {
    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).cov(Matrix(player1.timestamp.getTime), Matrix(player2.timestamp.getTime()))
    timeDiffCov * theSamePlayerCov
  }

  private def meanFunc(player: Player): Double = 0d

  private def covarianceMatrix_df_ell(players: Array[Player], ell: Double): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), ell))
  }

  private def covariance_df_ell(player1: Player, player2: Player, ell: Double): Double = {

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).df_dEll(Matrix(player1.timestamp.getTime), Matrix(player2.timestamp.getTime()))
    timeDiffCov * theSamePlayerCov
  }

}