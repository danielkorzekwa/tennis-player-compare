package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.gaussian.Gaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.api.Factor
import scala.math._

case class SkillsFactor(varId: Int, skillOnServe: Gaussian, skillOnReturn: Gaussian) extends SingleFactor {

  def getVariableId(): Int = varId

  def getVariableIds(): Seq[Int] = Vector(varId)

  def marginal(varId: Int): SingleFactor = throw new UnsupportedOperationException("Not implemented yet")

  def productMarginal(varId: Int, factor1: Factor, factor2: Factor, factor3: Factor): SingleFactor = throw new UnsupportedOperationException("Not implemented yet")

  override def /(factor: Factor): SkillsFactor = factor match {
    case factor: SkillsFactor => {
      val newSkillOnServe = skillOnServe / factor.skillOnServe
      val newSkillOnReturn = skillOnReturn / factor.skillOnReturn
      SkillsFactor(varId, newSkillOnServe, newSkillOnReturn)
    }
    case _ => throw new IllegalArgumentException("SkillsFactor factor cannot be divided by a factor that is non SkillsFactor")
  }

  override def *(factor: Factor): SkillsFactor = factor match {
    case factor: SkillsFactor => {
      val newSkillOnServe = skillOnServe * factor.skillOnServe
      val newSkillOnReturn = skillOnReturn * factor.skillOnReturn
      SkillsFactor(varId, newSkillOnServe, newSkillOnReturn)
    }
    case _ => throw new IllegalArgumentException("SkillsFactor factor cannot be multiplied by a factor that is non SkillsFactor")
  }

  override def equals(that: Factor, threshold: Double): Boolean = {

    that match {
      case that: SkillsFactor => varId == that.varId && compareGaussians(skillOnServe, that.skillOnServe, threshold) && compareGaussians(skillOnReturn, that.skillOnReturn, threshold)
      case _ => false
    }
  }

  private def compareGaussians(first: Gaussian, second: Gaussian, threshold: Double): Boolean = {
    (abs(first.m - second.m) < threshold && (abs(first.v - second.v) < threshold) || (first.v.isPosInfinity && second.v.isPosInfinity))
  }

}