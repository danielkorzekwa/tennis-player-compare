package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.SingleFactor

case class PriorSkillsFactor(skillsVarId: Int) extends SingleFactor {

  def getVariableId(): Int = skillsVarId

  def getVariableIds(): Seq[Int] = Vector(skillsVarId)

  def marginal(varId: Int): SingleFactor = throw new UnsupportedOperationException("Not implemented yet")
}