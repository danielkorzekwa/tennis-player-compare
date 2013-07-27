package dk.tennis.compare.rating.multiskill.domain

case class MultiSkillParams(
  skillOnServeTransVariance: Double, skillOnReturnTransVariance: Double,
  priorSkillOnServe: PlayerSkill, priorSkillOnReturn: PlayerSkill, perfVariance: Double) {

  require(skillOnServeTransVariance > 0, "Skill on serve transition variance must be greater than 0")
  require(skillOnReturnTransVariance > 0, "Skill on return transition variance must be greater than 0")
  require(perfVariance > 0, "Skill performance variance must be greater than 0")
}