package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.infer.gp.cov.CovSEiso
import scala.math._
import scala.Array.canBuildFrom
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.GPSkillMath
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.PlayerSkills
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.factorops.AllSkills

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

  private val DAY_MILLIS = 1000L * 3600 * 24

  private val priorSkillsMean = Matrix(players.map(p => meanFunc(p)))

  //add some noise on diagonal for numerical stability
  private val priorSkillsCov = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(ell)(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-1
  private val priorPlayerSkills = CanonicalGaussian(priorSkillsMean, priorSkillsCov)
  private val covD = covarianceMatrix_df_ell(players, ell)

  def calcPosteriorSkillsByPlayerMap2(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]):AllSkills = throw new UnsupportedOperationException("Not implemented yet")
  
   def calcPlayerSkill(player: Player, gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Gaussian = throw new UnsupportedOperationException("Not implemented yet")
  
   def getPriorSkillsForPlayer(playerName:String, skillOnServe:Boolean):MultivariateGaussian = throw new UnsupportedOperationException("Not implemented yet")
  
  def getGameSkillsMarginals(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[CanonicalGaussian] = {

    val skillsMarginalCanon = calcSkillsMarginal(gameSkillsVarUpMsgs)

    val skillsMarginal = MultivariateGaussian(skillsMarginalCanon.mean, skillsMarginalCanon.variance)
    val gameSkillsMarginals = (0 until gameSkillsVarUpMsgs.size).map { index =>

      val gameSkillsMarginal = GPSkillMath.getSkills(skillsMarginal, index)
      CanonicalGaussian(gameSkillsMarginal.m, gameSkillsMarginal.v)
    }

    gameSkillsMarginals
  }

  def getGameSkillsMarginalsD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Seq[MultivariateGaussian] = {

    val Kinv = priorPlayerSkills.variance.inv

    val skillsMarginal = calcSkillsMarginal(gameSkillsVarUpMsgs)

    val skillsMarginalVarD = skillsMarginal.variance * Kinv * covD * Kinv * skillsMarginal.variance
    val h_d = (-1 * Kinv * covD * Kinv) * priorPlayerSkills.mean
    val skillsMarginalMeanD = skillsMarginalVarD * skillsMarginal.h + skillsMarginal.variance * h_d

    val gameSkillsMarginalsD = (0 until gameSkillsVarUpMsgs.size).map { index =>

      val gameSkillsMarginalD = GPSkillMath.getSkills(MultivariateGaussian(skillsMarginalMeanD, skillsMarginalVarD), index)
      gameSkillsMarginalD
    }

    gameSkillsMarginalsD
  }

  def getGameSkillsMarginalsWithD(gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): Tuple2[Seq[CanonicalGaussian], Seq[Seq[MultivariateGaussian]]] = {
    throw new UnsupportedOperationException("Not implemented yet")
  }

  def getPlayerSkillsPriorMean(): Matrix = priorPlayerSkills.mean

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
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0
    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).cov(Matrix(player1.timestamp.getTime / DAY_MILLIS), Matrix(player2.timestamp.getTime / DAY_MILLIS))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

  private def meanFunc(player: Player): Double = 0d

  private def covarianceMatrix_df_ell(players: Array[Player], ell: Double): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), ell))
  }

  private def covariance_df_ell(player1: Player, player2: Player, ell: Double): Double = {
    val onServeCov = if (player1.onServe.equals(player2.onServe)) 1d else 0

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).df_dEll(Matrix(player1.timestamp.getTime / DAY_MILLIS), Matrix(player2.timestamp.getTime / DAY_MILLIS))
    onServeCov * theSamePlayerCov * timeDiffCov
  }

  def calcPosteriorSkillsForPlayer(playerName: String, skillOnServe: Boolean, gameSkillsVarUpMsgs: Seq[CanonicalGaussian]): PlayerSkills = throw new UnsupportedOperationException("Not implemented yet")

}