package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.TripleFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.gaussian.Gaussian

case class PlayerFactor(skillOnServeVarId: Int, skillOnReturnVarId: Int, playerVarId: Int) extends TripleFactor {

  def getVariableIds(): Seq[Int] = Vector(skillOnServeVarId, skillOnReturnVarId, playerVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `skillOnServeVarId` => new GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `skillOnReturnVarId` => new GaussianFactor(varId, 0, Double.PositiveInfinity)
      case `playerVarId` =>
        SkillsFactor(playerVarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def productMarginal(varId: Int, factor1: Factor, factor2: Factor, factor3: Factor): SingleFactor = {

    productMarginalInternal(varId, factor1.asInstanceOf[GaussianFactor], factor2.asInstanceOf[GaussianFactor], factor3.asInstanceOf[SkillsFactor])
  }
  private def productMarginalInternal(varId: Int, skillOnServe: GaussianFactor, skillOnReturn: GaussianFactor, playerSkills: SkillsFactor): SingleFactor = {

    val marginal = varId match {
      case `skillOnServeVarId` => {
        val skillOnServeGaussian = playerSkills.skillOnServe
        GaussianFactor(skillOnServeVarId, skillOnServeGaussian.m, skillOnServeGaussian.v) * skillOnServe
      }
      case `skillOnReturnVarId` => {
        val skillOnReturnGaussian = playerSkills.skillOnReturn
        GaussianFactor(skillOnReturnVarId, skillOnReturnGaussian.m, skillOnReturnGaussian.v) * skillOnReturn
      }
      case `playerVarId` => {
        val skillOnServeGaussian = Gaussian(skillOnServe.m,skillOnServe.v)
        val skillOnReturnGaussian =  Gaussian(skillOnReturn.m,skillOnReturn.v)
        SkillsFactor(playerSkills.varId, skillOnServeGaussian, skillOnReturnGaussian)
      }
      case _ => throw new IllegalArgumentException("Incorrect marginal variable id")
    }
    marginal
  }

}