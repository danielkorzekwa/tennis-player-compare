package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.DoubleFactor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.Factor

case class TransitionSkillsFactor(prevSkillsVarId: Int, skillsVarId: Int) extends DoubleFactor {

  def getVariableIds(): Seq[Int] = Vector(prevSkillsVarId, skillsVarId)

  def marginal(varId: Int): SingleFactor = throw new UnsupportedOperationException("Not implemented yet")

  def outgoingMessages(factor1: Factor, factor2: Factor): Tuple2[SingleFactor, SingleFactor] = throw new UnsupportedOperationException("Not implemented yet")
}