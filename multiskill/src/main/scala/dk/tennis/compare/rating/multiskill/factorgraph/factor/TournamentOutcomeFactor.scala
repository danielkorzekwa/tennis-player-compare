package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.Factor

case class TournamentOutcomeFactor(tournamentVarId: Int) extends SingleFactor {

  def getVariableId(): Int = tournamentVarId

  def getVariableIds(): Seq[Int] = Vector(tournamentVarId)

  def marginal(varId: Int): TournamentOutcomeFactor = {
    require(varId == tournamentVarId, "Wrong varId")
    this
  }

  override def /(factor: Factor): TournamentOutcomeFactor = {
    factor match {
      case factor: TournamentOutcomeFactor => {
        this
      }
      case _ => throw new IllegalArgumentException("TournamentOutcomeFactor factor cannot be divided by a factor that is non TournamentOutcomeFactor")
    }
  }

  override def equals(that: Factor, threshold: Double): Boolean = true
}