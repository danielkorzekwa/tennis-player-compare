package dk.tennis.compare.rating.multiskill.model.career.tournament

import dk.bayes.model.factor.api.TripleFactor
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear._
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.api.Factor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.SkillsFactor
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factor.api.DoubleFactor
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

/**
 * @param players
 * @param skillsFactor Skills on serve and return factor
 * @param outcomeVarId
 */
 class TennisMatchCorFactor(
  player1: String, player2: String, servicePointsWon: Int, servicePointsTotal: Int,
  allPlayers: IndexedSeq[String], skillsFactor: MvnGaussianFactor, outcomeVarId: Int,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends DoubleFactor {

  val skillsVarId = skillsFactor.varId

  def getVariableIds(): Seq[Int] = Vector(skillsVarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `skillsVarId` =>
        MvnGaussianFactor(skillsVarId, CanonicalGaussian(
          Matrix.zeros(allPlayers.size * 2, 1),
          Matrix(allPlayers.size * 2, allPlayers.size * 2, (row: Int, col: Int) => Double.PositiveInfinity)))

      case `outcomeVarId` => SingleTableFactor(outcomeVarId, 2, Array(1, 1))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def outgoingMessages(factor1: Factor, factor2: Factor): Tuple2[MvnGaussianFactor, SingleTableFactor] = {
    outgoingMessagesInternal(factor1.asInstanceOf[MvnGaussianFactor], factor2.asInstanceOf[SingleTableFactor])
  }

  def outgoingMessagesInternal(skillsFactor: MvnGaussianFactor, outcomeFactor: SingleTableFactor): Tuple2[MvnGaussianFactor, SingleTableFactor] = {
    val p1Index = allPlayers.indexOf(player1)
    val p2Index = allPlayers.size + allPlayers.indexOf(player2)
    val a = Matrix(2, 2 * allPlayers.size, Array.fill(2 * 2 * allPlayers.size)(0d))
    a.set(0, p1Index, 1)
    a.set(1, p2Index, 1)
    val directSkillsFactor = CanonicalGaussian(a, b = Matrix(0, 0), v = Matrix(2, 2, Array(1e-10, 1e-12, 1e-12, 1e-10)))

    val directSkills_to_outcome_msg = (skillsFactor.canonGaussian.extend(2 * allPlayers.size + 2, 0) * directSkillsFactor).marginal(2 * allPlayers.size, 2 * allPlayers.size + 1)

    val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

    val newDirectSkills = model.skillMarginals(directSkills_to_outcome_msg, servicePointsWon, servicePointsTotal)
    val directSkillsMsg = newDirectSkills / directSkills_to_outcome_msg

    val skillsMsg = (directSkillsFactor * directSkillsMsg.extend(2 * allPlayers.size + 2, 2 * allPlayers.size)).marginalise(2 * allPlayers.size + 1).marginalise(2 * allPlayers.size)
    val outcomeMsg = SingleTableFactor(outcomeVarId, 2, Array(1, 1))
    Tuple2(MvnGaussianFactor(skillsFactor.varId,skillsMsg), outcomeMsg)
  }
}