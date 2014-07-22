package dk.tennis.compare.rating.multiskill.model.gpskill.naive
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.linear.Matrix
import scala.math._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.gpskill.Player
import dk.tennis.compare.rating.multiskill.model.gpskill.Score
import scala.Array.canBuildFrom
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.naive.factorgraph.GPSkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.gpskill.naive.factorgraph.calibrate
import dk.bayes.infer.gp.cov.CovSEiso

/**
 * @param hyp (log of length scale of game timestamp)
 */
case class NaiveGPSkills(hyp: Array[Double], perfVarOnServe: Double, perfVarOnReturn: Double, players: Array[Player], scores: Array[Score],
  threshold: Double = 1e-4) extends GPSkills with Logging {

  val ell = hyp(0)

  private def meanFunc(player: Player): Double = 0d

  private def covariance(ell: Double)(player1: Player, player2: Player): Double = {
    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).cov(Matrix(player1.timestamp.getTime), Matrix(player2.timestamp.getTime()))
    timeDiffCov * theSamePlayerCov
  }

  private val priorSkillsMean = Matrix(players.map(p => meanFunc(p)))

  //add some noise on diagonal for numerical stability
  val priorSkillsCov = Matrix(players.size, players.size, (rowIndex, colIndex) => covariance(ell)(players(rowIndex), players(colIndex))) + Matrix.identity(players.size) * 1e-1

  private val skillsFactorGraph = calibrateSkillsFactorGraph()

  def getSkillsMarginal(): MultivariateGaussian = skillsFactorGraph.getPlayerSkillsMarginal

  def playerSkill(player: Player): Gaussian = {

    val kZX = Matrix(1, players.size, (rowIndex, colIndex) => covariance(ell)(player, players(colIndex)))
    val kXXinv = priorSkillsCov.inv
    val kZZ = Matrix(covariance(ell)(player, player))

    val A = kZX * kXXinv

    val condVar = kZZ - kZX * kXXinv * kZX.t + Matrix(1e-10) //add some noise to avoid 0 variance for player from the training set
    val linearGaussian = CanonicalGaussian(A, Matrix(0), condVar)

    val playerSkillsMarginal = skillsFactorGraph.getPlayerSkillsMarginal()
    val playerMarginal = (CanonicalGaussian(playerSkillsMarginal.m, playerSkillsMarginal.v).extend(playerSkillsMarginal.m.size + 1, 0) * linearGaussian).marginal(playerSkillsMarginal.m.size)

    playerMarginal.toGaussian
  }

  private def calibrateSkillsFactorGraph(): GPSkillsFactorGraph = {

    logger.info("Creating factor graph")
    val priorPlayerSkills = MultivariateGaussian(priorSkillsMean, priorSkillsCov)
    val factorGraph = GPSkillsFactorGraph(priorPlayerSkills, scores, perfVarOnServe, perfVarOnReturn)

    calibrate(factorGraph, threshold)
    factorGraph
  }

  def loglik(): Double = {

    // points won on serve by (player1,player2)
    val gameResults = toGameResults(scores)

    val logLiks = gameResults.zipWithIndex.map {
      case (result, index) =>

        val skillDiff = getSkillDiff(index)
        val loglik = result._1 * OutcomeLik.loglik(skillDiff, true) + result._2 * OutcomeLik.loglik(skillDiff, false)
        loglik
    }

    logLiks.sum
  }

  def loglikD(): Array[Double] = {
    val covD = covarianceMatrix_df_ell(players, ell)
    val loglikDValue = loglikD(covD)

    Array(loglikDValue)

  }
  def covarianceMatrix_df_ell(players: Array[Player], ell: Double): Matrix = {
    Matrix(players.size, players.size, (rowIndex, colIndex) => covariance_df_ell(players(rowIndex), players(colIndex), ell))
  }
  private def covariance_df_ell(player1: Player, player2: Player, ell: Double): Double = {

    val theSamePlayerCov = if (player1.playerName.equals(player2.playerName)) 1d else 0
    val timeDiffCov = new CovSEiso(sf = log(1), ell).df_dEll(Matrix(player1.timestamp.getTime), Matrix(player2.timestamp.getTime()))
    timeDiffCov * theSamePlayerCov
  }

  def loglikD(covD: Matrix): Double = {
    // points won on serve by (player1,player2)
    val gameResults = toGameResults(scores)

    val logLiksD = gameResults.zipWithIndex.map {
      case (result, index) =>

        val skillDiff = getSkillDiff(index)
        val (muD, varD) = getSkillDiffD(covD, index)

        OutcomeLik.loglikD(skillDiff, true, muD, varD)
        val loglikD = result._1 * OutcomeLik.loglikD(skillDiff, true, muD, varD) + result._2 * OutcomeLik.loglikD(skillDiff, false, muD, varD)
        loglikD
    }

    logLiksD.sum
  }

  private def getSkillDiff(gameIndex: Int): Gaussian = {
    val C = getC(skillsFactorGraph, gameIndex) //cavity
    val C_d = GPSkillMath.getSkills(C, gameIndex) //direct skills
    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(perfVarOnServe, 0, 0, perfVarOnReturn))

    val skillDiff = MultivariateGaussian((A_d * C_d.m), (A_d * (C_d.v + V_d) * A_d.t)).toGaussian

    skillDiff
  }

  /**
   * Returns partial derivatives of skill difff [mu,var] with respect to some parameter
   */
  private def getSkillDiffD(covD: Matrix, gameIndex: Int): Tuple2[Double, Double] = {
    val skillDiff = getSkillDiff(gameIndex)

    val C = getC(skillsFactorGraph, gameIndex) //cavity

    val dC = get_dC(C, skillsFactorGraph.priorPlayerSkills, covD) //cavity derivatives 
    val dC_d = GPSkillMath.getSkills(dC, gameIndex) // direct skills of cavity derivatives

    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(perfVarOnServe, 0, 0, perfVarOnReturn))

    val muD = (A_d * dC_d.m).at(0)
    val varD = (A_d * dC_d.v * A_d.t).at(0)

    (muD, varD)
  }

  /**
   *  Returns skills cavity distribution
   */
  private def getC(skillsFactorGraph: GPSkillsFactorGraph, gameIndex: Int): CanonicalGaussian = {
    val skillsGaussianMarginal = skillsFactorGraph.getPlayerSkillsMarginal
    //obtain cavity distribution
    val canonSkillsMarginal = CanonicalGaussian(skillsGaussianMarginal.m, skillsGaussianMarginal.v)
    val gameToSkillMsg = skillsFactorGraph.gameToSkillsFactorMsgs(gameIndex)
    GPSkillMath.updateProduct(canonSkillsMarginal, gameToSkillMsg, gameIndex, false)

    canonSkillsMarginal
  }

  /**
   *  Returns partial derivative of skills cavity distribution with respect to some parameter
   *
   *  @param P - skills prior
   *  @param C cavity distribution
   *  @param covD Element wise partial derivatives of skills covariance matrix with respect to some parameter
   */
  private def get_dC(C: CanonicalGaussian, P: MultivariateGaussian, covD: Matrix): MultivariateGaussian = {

    val Kinv = P.v.inv
    val dC_var = C.variance * Kinv * covD * Kinv * C.variance
    val dC_mean = dC_var * C.h

    MultivariateGaussian(dC_mean, dC_var)

  }

  /**
   * Returns points won on serve by (player1,player2)
   */
  private def toGameResults(scores: Seq[Score]): Seq[Tuple2[Int, Int]] = {

    val pointStatsOnServe = scores.map { s => (s.p1PointsWon, s.p2PointsWon) }

    pointStatsOnServe.toList
  }

  private implicit def MvnGaussian(canon: CanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }
}