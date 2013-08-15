package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.LinearGaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.TripleFactor
import dk.tennis.compare.rating.multiskill.matchmodel.online.GenericOnlineMatchModel
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.tennis.compare.rating.multiskill.matchmodel.dbn.GenericDbnMatchModel

case class TennisMatchFactor(p1Factor: PlayerFactor, p2Factor: PlayerFactor, outcomeVarId: Int,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double, matchResult: MatchResult) extends TripleFactor {

  private val p1VarId = p1Factor.playerVarId
  private val p2VarId = p2Factor.playerVarId

  private val ZERO_PROBABILITY = 1.0E-20

  def getVariableIds(): Seq[Int] = Vector(p1VarId, p2VarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `p1VarId` => SkillsFactor(p1VarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `p2VarId` => SkillsFactor(p2VarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `outcomeVarId` => SingleTableFactor(outcomeVarId, 2, Array(1, 1))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def outgoingMessages(factor1: Factor, factor2: Factor, factor3: Factor): Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = {
    outgoingMessagesInternal(factor1.asInstanceOf[SkillsFactor], factor2.asInstanceOf[SkillsFactor], factor3.asInstanceOf[SingleTableFactor])
  }

  def outgoingMessagesInternal(p1Skills: SkillsFactor, p2Skills: SkillsFactor, outcomeFactor: SingleTableFactor): Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = {
    val initialP1Skills = toPlayerSkills(matchResult.player1, p1Skills)
    val initialP2SKills = toPlayerSkills(matchResult.player2, p2Skills)

    val matchModel = GenericOnlineMatchModel(initialP1Skills, initialP2SKills, perfVarianceOnServe, perfVarianceOnReturn)
    matchResult.pointResults.foreach(p => matchModel.onPoint(p))

    val newP1Skills = matchModel.getP1Skills()
    val newP2Skills = matchModel.getP2Skills()
    val outcomeMsg = SingleTableFactor(outcomeVarId, 2, Array(1, 1))

    val p1SkillsMsg = toFactor(p1VarId, newP1Skills) / p1Skills
    val p2SkillsMsg = toFactor(p2VarId, newP2Skills) / p2Skills

    Tuple3(p1SkillsMsg, p2SkillsMsg, outcomeMsg)
  }

  private def toFactor(varId: Int, playerSkills: PlayerSkills): SkillsFactor = {
    SkillsFactor(varId,
      Gaussian(playerSkills.skillOnServe.mean, playerSkills.skillOnServe.variance),
      Gaussian(playerSkills.skillOnReturn.mean, playerSkills.skillOnReturn.variance))
  }

  private def toPlayerSkills(player: String, skillsFactor: SkillsFactor): PlayerSkills = {
    val playerSkills = PlayerSkills(player,
      PlayerSkill(skillsFactor.skillOnServe.m, skillsFactor.skillOnServe.v),
      PlayerSkill(skillsFactor.skillOnReturn.m, skillsFactor.skillOnReturn.v))
    playerSkills
  }

}