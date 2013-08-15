package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor

case class TournamentFactor extends Factor {

  def getVariableIds(): Seq[Int] = throw new UnsupportedOperationException("Not implemented yet")

  def marginal(varId: Int): SingleFactor = throw new UnsupportedOperationException("Not implemented yet")
}