package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

case class PriorSkillsFactor(skillsVarId: Int, skillOnServe: Gaussian, skillOnReturn: Gaussian) extends SingleFactor {

  def getVariableId(): Int = skillsVarId

  def getVariableIds(): Seq[Int] = Vector(skillsVarId)

  def marginal(varId: Int): PriorSkillsFactor = {
    require(varId == skillsVarId, "Wrong varId")
    this.copy()
  }

  override def *(factor: Factor): PriorSkillsFactor = {
    factor match {
      case factor: PriorSkillsFactor => {
        val newSkillOnServe = skillOnServe * factor.skillOnServe
        val newSkillOnReturn = skillOnReturn * factor.skillOnReturn
        PriorSkillsFactor(skillsVarId, newSkillOnServe, newSkillOnReturn)
      }
      case _ => throw new IllegalArgumentException("PriorSkillsFactor factor cannot be multiplied by a factor that is non PriorSkillsFactor")
    }
  }

  override def /(factor: Factor): PriorSkillsFactor = {
    factor match {
      case factor: PriorSkillsFactor => {
        val newSkillOnServe = skillOnServe / factor.skillOnServe
        val newSkillOnReturn = skillOnReturn / factor.skillOnReturn
        PriorSkillsFactor(skillsVarId, newSkillOnServe, newSkillOnReturn)
      }
      case _ => throw new IllegalArgumentException("PriorSkillsFactor factor cannot be divided by a factor that is non PriorSkillsFactor")
    }
  }

  override def equals(that: Factor, threshold: Double): Boolean = {

   val thesame =  that match {
      case that: PriorSkillsFactor => skillsVarId == that.skillsVarId && compareGaussians(skillOnServe, that.skillOnServe, threshold) && compareGaussians(skillOnReturn, that.skillOnReturn, threshold)
      case _ => false
    }
    thesame
  }

  def toPlayerSkills(player: String): PlayerSkills =
    PlayerSkills(player, PlayerSkill(skillOnServe.m, skillOnServe.v), PlayerSkill(skillOnReturn.m, skillOnReturn.v))

  private def compareGaussians(first: Gaussian, second: Gaussian, threshold: Double): Boolean = {
    val thesame = (abs(first.m - second.m) < threshold && (abs(first.v - second.v) < threshold) || (first.v.isPosInfinity && second.v.isPosInfinity))

    thesame
  }
}