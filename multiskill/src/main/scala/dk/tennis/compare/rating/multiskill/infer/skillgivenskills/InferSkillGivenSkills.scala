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
private val mleSkillMean = (x.m.toArray.sum/x.m.toArray.size)

  private val xPriorSkillMean = Matrix(players.map(p => mleSkillMean))
  private val xPriorSkillVarInv = playerCovFunc.covarianceMatrix(players).inv
  private val KxxInv = x.v.inv

  def infer(z: Player): Gaussian = {

    /** A and Kz_x parameters for p(z|x)*/
    val Kzx = playerCovFunc.covarianceMatrix(Array(z), players)

    val Kzz = playerCovFunc.covarianceMatrix(Array(z))

    val zSkillMean = Matrix(mleSkillMean)

   
    val skill = inferMarginalOfZ(xPriorSkillMean, xPriorSkillVarInv, x.m, x.v, zSkillMean, Kzz, Kzx)

    skill
  }

}

