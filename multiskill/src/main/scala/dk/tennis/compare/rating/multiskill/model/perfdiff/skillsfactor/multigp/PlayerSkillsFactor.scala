package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.doubleToLinearDouble
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.GPSkillMath
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFunc

case class PlayerSkillsFactor(meanFunc: Player => Double, playerCovFunc: PlayerCovFunc, players: Array[Player]) {

  private val priorSkillsMean = Matrix(players.map(p => meanFunc(p)))

  //add some noise on diagonal for numerical stability
  private val priorSkillsCov = playerCovFunc.covarianceMatrix(players)

  private val Kinv = priorSkillsCov.inv

  val priorPlayerSkills = PlayerSkills(MultivariateGaussian(priorSkillsMean, priorSkillsCov), players)

  def skillPosterior(skillVarUpMsgs: Array[CanonicalGaussian]): CanonicalGaussian = {

    //prior * likelihood
    val playerskillsMarginal = CanonicalGaussian(priorSkillsMean, priorSkillsCov)

    skillVarUpMsgs.zipWithIndex.foreach {
      case (msg, index) =>
        GPSkillMath.updateOnPlayerMsg(playerskillsMarginal, msg, index, true)
    }

    playerskillsMarginal
  }

  def skillPosteriorD(skillPosterior: MultivariateGaussian): Seq[PlayerSkills] = {

    def calcSkillMarginalD(covD: Matrix): MultivariateGaussian = {
      val skillsMarginalVarD = skillPosterior.v * Kinv * covD * Kinv * skillPosterior.v
      val h_d = (-1 * Kinv * covD * Kinv) * priorSkillsMean
      val skillsMarginalMeanD = skillsMarginalVarD * (skillPosterior.v.inv * skillPosterior.m) + skillPosterior.v * h_d

      MultivariateGaussian(skillsMarginalMeanD, skillsMarginalVarD)

    }

    val covDs: Seq[Matrix] = playerCovFunc.covarianceMatrixD(players)

    val skillPosteriorD = covDs.map(covD => PlayerSkills(calcSkillMarginalD(covD), players))
    skillPosteriorD
  }
}