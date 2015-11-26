package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor

import scala.Array.canBuildFrom
import breeze.linalg._
import breeze.linalg.DenseMatrix
import breeze.linalg.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.GPSkillMath
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.bayes.math.linear.invchol

case class PlayerSkillsFactor(meanFunc: Player => Double, playerCovFunc: CovFunc, players: Array[Player]) {

  private val priorSkillsMean = DenseVector(players.map(p => meanFunc(p)))

  //add some noise on diagonal for numerical stability
  private val priorSkillsCov = playerCovFunc.covarianceMatrix(players)

  private val Kinv = invchol(cholesky(priorSkillsCov).t)

  val priorPlayerSkills = PlayerSkills(MultivariateGaussian(priorSkillsMean, priorSkillsCov), players)

  def skillPosterior(skillVarUpMsgs: Array[DenseCanonicalGaussian]): DenseCanonicalGaussian = {

    //prior * likelihood
    val playerskillsMarginal = DenseCanonicalGaussian(priorSkillsMean, priorSkillsCov)

    skillVarUpMsgs.zipWithIndex.foreach {
      case (msg, index) =>
        GPSkillMath.updateOnPlayerMsg(playerskillsMarginal, msg, index, true)
    }

    playerskillsMarginal
  }

  def skillPosteriorD(skillPosterior: MultivariateGaussian): Seq[PlayerSkills] = {

    def calcSkillMarginalD(covD: DenseMatrix[Double]): MultivariateGaussian = {
      val skillsMarginalVarD = skillPosterior.v * Kinv * covD * Kinv * skillPosterior.v
      val h_d = (-1d * Kinv * covD * Kinv) * priorSkillsMean
      val skillsMarginalMeanD = skillsMarginalVarD * (invchol(cholesky(skillPosterior.v).t) * skillPosterior.m) + skillPosterior.v * h_d

      MultivariateGaussian(skillsMarginalMeanD, skillsMarginalVarD)

    }

    val covDs: Seq[DenseMatrix[Double]] = (0 until playerCovFunc.getParams().size).map { i =>
     DenseMatrix.tabulate(players.size, players.size){case (rowIndex, colIndex) => playerCovFunc.covarianceD(players(rowIndex), players(colIndex), i)}
    }

    val skillPosteriorD = covDs.map(covD => PlayerSkills(calcSkillMarginalD(covD), players))
    skillPosteriorD
  }
}