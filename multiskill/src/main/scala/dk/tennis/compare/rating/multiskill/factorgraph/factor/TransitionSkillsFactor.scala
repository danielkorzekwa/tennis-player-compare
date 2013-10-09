package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.DoubleFactor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.math.gaussian.Gaussian

case class TransitionSkillsFactor(prevSkillsVarId: Int, skillsVarId: Int, skillOnServeTransVariance: Double, skillOnReturnTransVariance: Double) extends DoubleFactor {

  def getVariableIds(): Seq[Int] = Vector(prevSkillsVarId, skillsVarId)

  def marginal(varId: Int): PriorSkillsFactor = PriorSkillsFactor(varId, Gaussian(0, Double.PositiveInfinity), Gaussian(0, Double.PositiveInfinity))

  def outgoingMessages(prevSkillsFactor: Factor, skillsFactor: Factor): Tuple2[PriorSkillsFactor, PriorSkillsFactor] = {
    outgoingMessagesInternal(prevSkillsFactor.asInstanceOf[PriorSkillsFactor], skillsFactor.asInstanceOf[PriorSkillsFactor])
  }

  private def outgoingMessagesInternal(prevSkillsFactor: PriorSkillsFactor, skillsFactor: PriorSkillsFactor): Tuple2[PriorSkillsFactor, PriorSkillsFactor] = {

    val prevSkillsMsg = PriorSkillsFactor(prevSkillsVarId,
      skillsFactor.skillOnServe.copy(v = skillsFactor.skillOnServe.v + skillOnServeTransVariance),
      skillsFactor.skillOnReturn.copy(v = skillsFactor.skillOnReturn.v + skillOnReturnTransVariance))

    val skillsMsg = PriorSkillsFactor(skillsVarId,
      prevSkillsFactor.skillOnServe.copy(v = prevSkillsFactor.skillOnServe.v + skillOnServeTransVariance),
      prevSkillsFactor.skillOnReturn.copy(v = prevSkillsFactor.skillOnReturn.v + skillOnReturnTransVariance))

    Tuple2(prevSkillsMsg, skillsMsg)
  }
}