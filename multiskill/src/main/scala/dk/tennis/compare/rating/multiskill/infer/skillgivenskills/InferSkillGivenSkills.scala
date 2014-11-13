package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import InferSkillGivenSkills._

/**
 * Infer player skill given other players skills
 */
case class InferSkillGivenSkills(playerSkills: Option[PlayerSkills], playerCovFunc: CovFunc, skillMeanFunc: (Player) => Double) {

  val skillsPrior = playerSkills match {
    case Some(playerSkills) => Some(SkillsPrior(playerSkills, playerCovFunc, skillMeanFunc))
    case None => None
  }
  def infer(z: Player): Gaussian = {

    val Kzz = playerCovFunc.covarianceMatrix(Array(z))
    val zSkillMean = Matrix(skillMeanFunc(z))

    val skill = skillsPrior match {
      case Some(skillsPrior) => {

        /** A and Kz_x parameters for p(z|x)*/
        val Kzx = playerCovFunc.covarianceMatrix(Array(z), skillsPrior.players)

        val skill = inferMarginalOfZ(skillsPrior.xPriorSkillMean, skillsPrior.xPriorSkillVarInv, skillsPrior.x.m, skillsPrior.x.v, zSkillMean, Kzz, Kzx)

        skill
      }

      case None => {
        Gaussian(zSkillMean(0), Kzz(0))
      }

    }

    skill
  }

}

object InferSkillGivenSkills {

  case class SkillsPrior(playerSkills: PlayerSkills, playerCovFunc: CovFunc, skillMeanFunc: (Player) => Double) {
    val players = playerSkills.players
    val x = playerSkills.skillsGaussian

    val xPriorSkillMean = Matrix(players.map(p => skillMeanFunc(p)))
    val xPriorSkillVarInv = playerCovFunc.covarianceMatrix(players).inv
    val KxxInv = x.v.inv
  }
}

