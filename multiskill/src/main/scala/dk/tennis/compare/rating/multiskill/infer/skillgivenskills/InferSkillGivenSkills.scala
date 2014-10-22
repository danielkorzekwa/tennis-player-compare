package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills

/**
 * Infer player skill given other players skills
 */
case class InferSkillGivenSkills(playerSkills: PlayerSkills, playerCovFunc: CovFunc, skillMeanFunc: (Player) => Double) {

  private val players = playerSkills.players
  private val x = playerSkills.skillsGaussian

  private val xSkillMean = Matrix(players.map(p => skillMeanFunc(p)))
  private val KxxInv = x.v.inv

  def infer(z: Player): Gaussian = {

    /** A and Kz_x parameters for p(z|x)*/
    val Kzx = playerCovFunc.covarianceMatrix(Array(z), players)
    val Kxz = Kzx.t

    val Kzz = playerCovFunc.covarianceMatrix(Array(z))

    val zSkillMean = Matrix(skillMeanFunc(z))

    val A = Kzx * KxxInv

    val Kz_x = Kzz - Kzx * KxxInv * Kxz

    /** RUN INFERENCE - Compute p(z) = integral of p(x)*p(z|x)dx*/
    val skillMean = zSkillMean + A * (x.m - xSkillMean)
    val skillVar = Kz_x + A * x.v * A.t

    Gaussian(skillMean(0), skillVar(0))
  }

}