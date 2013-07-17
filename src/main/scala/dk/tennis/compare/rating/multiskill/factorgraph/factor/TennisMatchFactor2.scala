package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.TripleFactor
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel
import dk.tennis.compare.rating.multiskill.matchmodel.GenericMatchModel
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

case class TennisMatchFactor2(p1SkillsVarId: Int, p2SkillsVarId: Int, outcomeVarId: Int,
  perfVariance: Double, player1: String, player2: String, pointResults: Seq[PointResult]) extends TripleFactor {

  private val ZERO_PROBABILITY = 1.0E-20

  def getVariableIds(): Seq[Int] = Vector(p1SkillsVarId, p2SkillsVarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `p1SkillsVarId` => SkillsFactor(p1SkillsVarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `p2SkillsVarId` => SkillsFactor(p2SkillsVarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `outcomeVarId` => SingleTableFactor(outcomeVarId, 2, Array(1, 1))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def productMarginal(varId: Int, factor1: Factor, factor2: Factor, factor3: Factor): SingleFactor = {
    productMarginalInternal(varId, factor1.asInstanceOf[SkillsFactor], factor2.asInstanceOf[SkillsFactor], factor3.asInstanceOf[SingleTableFactor])
  }

  private def productMarginalInternal(varId: Int, p1Skills: SkillsFactor, p2Skills: SkillsFactor, outcomeFactor: SingleTableFactor): SingleFactor = {

    val initialP1Skills = PlayerSkills(player1, PlayerSkill(p1Skills.skillOnServe.m, p1Skills.skillOnServe.v), PlayerSkill(p1Skills.skillOnReturn.m, p1Skills.skillOnReturn.v))
    val initialP2SKills = PlayerSkills(player2, PlayerSkill(p2Skills.skillOnServe.m, p2Skills.skillOnServe.v), PlayerSkill(p2Skills.skillOnReturn.m, p2Skills.skillOnReturn.v))
    val matchModel = GenericMatchModel(initialP1Skills, initialP2SKills, perfVariance)
    pointResults.foreach(p => matchModel.onPoint(p))

    val newP1Skills = matchModel.getP1Skills()
    val newP2Skills = matchModel.getP2Skills()

    val marginal = varId match {
      case `p1SkillsVarId` => SkillsFactor(p1SkillsVarId,
        Gaussian(newP1Skills.skillOnServe.mean, newP1Skills.skillOnServe.variance),
        Gaussian(newP1Skills.skillOnReturn.mean, newP1Skills.skillOnReturn.variance))
      case `p2SkillsVarId` => SkillsFactor(p2SkillsVarId,
        Gaussian(newP2Skills.skillOnServe.mean, newP2Skills.skillOnServe.variance),
        Gaussian(newP2Skills.skillOnReturn.mean, newP2Skills.skillOnReturn.variance))
      case `outcomeVarId` => SingleTableFactor(outcomeVarId, 2, Array(1, 1))
      case _ => throw new IllegalArgumentException("Incorrect marginal variable id")
    }

    marginal
  }

}