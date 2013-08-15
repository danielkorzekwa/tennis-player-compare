package dk.tennis.compare.rating.multiskill.domain

case class MultiSkillParams(
  skillOnServeTransVariance: Double, skillOnReturnTransVariance: Double,
  priorSkillOnServe: PlayerSkill, priorSkillOnReturn: PlayerSkill, perfVarianceOnServe: Double, perfVarianceOnReturn: Double) {

  require(skillOnServeTransVariance > 0, "Skill on serve transition variance must be greater than 0")
  require(skillOnReturnTransVariance > 0, "Skill on return transition variance must be greater than 0")
  require(perfVarianceOnServe > 0, "Skill on serve performance variance must be greater than 0")
  require(perfVarianceOnReturn > 0, "Skill on return performance variance must be greater than 0")
}