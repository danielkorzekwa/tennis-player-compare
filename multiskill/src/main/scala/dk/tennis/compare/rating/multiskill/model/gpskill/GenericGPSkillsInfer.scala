package dk.tennis.compare.rating.multiskill.model.gpskill
import scala.annotation.tailrec
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph.GPSkillsFactorGraph
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph.GPSkillsFactorGraph
import scala.math._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik

case class GenericGPSkillsInfer(perfVarOnServe: Double, perfVarOnReturn: Double, players: Array[Player]) extends GPSkillsInfer with Logging {

  private val (pointPerfVarianceOnServe, pointPerfVarianceOnReturn) = (195.61914136268837, 155)

  def skillsMarginal(priorPlayerSkills: MultivariateGaussian, threshold: Double = 1e-4): GPSkillsFactorGraph = {

    logger.info("Creating factor graph")
    val factorGraph = GPSkillsFactorGraph(priorPlayerSkills, players, perfVarOnServe, perfVarOnReturn)

    @tailrec
    def calibrate(playerSkillsMarginal: MultivariateGaussian) {
      logger.info("Calibrating tournament model...")
      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginal()

      if (equals(newSkillsMarginal, playerSkillsMarginal, threshold)) return
      else calibrate(newSkillsMarginal)
    }

    calibrate(priorPlayerSkills)
    factorGraph

  }

  def loglik(skillsFactorGraph: GPSkillsFactorGraph): Double = {

    // points won on serve by (player1,player2)
    val gameResults = toGameResults(players)

    val logLiks = gameResults.zipWithIndex.map {
      case (result, index) =>

        val skillDiff = getSkillDiff(skillsFactorGraph, index)
        val loglik = result._1 * OutcomeLik.loglik(skillDiff, true) + result._2 * OutcomeLik.loglik(skillDiff, false)
        loglik
    }

    logLiks.sum
  }

  def loglikD(skillsFactorGraph: GPSkillsFactorGraph, covD: Matrix): Double = {
    // points won on serve by (player1,player2)
    val gameResults = toGameResults(players)

    val logLiksD = gameResults.zipWithIndex.map {
      case (result, index) =>

        val skillDiff = getSkillDiff(skillsFactorGraph, index)
        val (muD, varD) = getSkillDiffD(skillsFactorGraph, covD, index)

        OutcomeLik.loglikD(skillDiff, true, muD, varD)
        val loglikD = result._1 * OutcomeLik.loglikD(skillDiff, true, muD, varD) + result._2 * OutcomeLik.loglikD(skillDiff, false, muD, varD)
        loglikD
    }

    logLiksD.sum
  }

  private def getSkillDiff(skillsFactorGraph: GPSkillsFactorGraph, gameIndex: Int): Gaussian = {
    val C = getC(skillsFactorGraph, gameIndex) //cavity
    val C_d = GPSkillMath.getSkills(C, gameIndex) //direct skills
    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(pointPerfVarianceOnServe, 0, 0, pointPerfVarianceOnReturn))

    val skillDiff = MultivariateGaussian((A_d * C_d.m), (A_d * (C_d.v + V_d) * A_d.t)).toGaussian

    skillDiff
  }

  /**
   * Returns partial derivatives of skill difff [mu,var] with respect to some parameter
   */
  private def getSkillDiffD(skillsFactorGraph: GPSkillsFactorGraph, covD: Matrix, gameIndex: Int): Tuple2[Double, Double] = {
    val skillDiff = getSkillDiff(skillsFactorGraph, gameIndex)

    val C = getC(skillsFactorGraph, gameIndex) //cavity

    val dC = get_dC(C, skillsFactorGraph.priorPlayerSkills, covD) //cavity derivatives 
    val dC_d = GPSkillMath.getSkills(dC, gameIndex) // direct skills of cavity derivatives

    val A_d = Matrix(1d, -1d).t
    val V_d = Matrix(2, 2, Array(pointPerfVarianceOnServe, 0, 0, pointPerfVarianceOnReturn))

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
  private def toGameResults(players: Seq[Player]): Seq[Tuple2[Int, Int]] = {

    val pointStatsOnServe = players.grouped(2).map {
      case Seq(pOnServe, pOnReturn) =>
        (pOnServe.pointsWon, pOnReturn.pointsWon)
    }

    pointStatsOnServe.toList
  }

  def equals(gaussian1: MultivariateGaussian, gaussian2: MultivariateGaussian, threshold: Double): Boolean = {

    val (mean1, variance1) = (gaussian1.m, gaussian1.v)
    val (mean2, variance2) = (gaussian2.m, gaussian2.v)

    mean1.matrix.isIdentical(mean2.matrix, threshold)
    // variance1.matrix.isIdentical(variance2.matrix, threshold)

  }

  private implicit def MvnGaussian(canon: CanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }
}