package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.TripleFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.math.gaussian.Gaussian

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

  def outgoingMessages(factor1: Factor, factor2: Factor, factor3: Factor): Tuple3[GaussianFactor, GaussianFactor, SkillsFactor] = {
    outgoingMessagesInternal(factor1.asInstanceOf[GaussianFactor], factor2.asInstanceOf[GaussianFactor], factor3.asInstanceOf[SkillsFactor])
  }

  private def outgoingMessagesInternal(skillOnServe: GaussianFactor, skillOnReturn: GaussianFactor, playerSkills: SkillsFactor): Tuple3[GaussianFactor, GaussianFactor, SkillsFactor] = {

    val skillOnServeGaussian = playerSkills.skillOnServe
    val skillOnServeMsg = GaussianFactor(skillOnServeVarId, skillOnServeGaussian.m, skillOnServeGaussian.v)

    val skillOnReturnGaussian = playerSkills.skillOnReturn
    val skillOnReturnMsg = GaussianFactor(skillOnReturnVarId, skillOnReturnGaussian.m, skillOnReturnGaussian.v)

    val playerSkillsMsg = SkillsFactor(playerVarId, Gaussian(skillOnServe.m, skillOnServe.v), Gaussian(skillOnReturn.m, skillOnReturn.v))

    Tuple3(skillOnServeMsg, skillOnReturnMsg, playerSkillsMsg)

  }

}