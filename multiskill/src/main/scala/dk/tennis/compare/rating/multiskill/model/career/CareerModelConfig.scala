package dk.tennis.compare.rating.multiskill.model.career

import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

case class CareerModelConfig(initialSkillsOnServe: PlayerSkill, initialSkillsOnReturn: PlayerSkill,
  skillOnServeTransVariance: Double, skillOnReturnTransVariance: Double,
  pointPerfVarianceOnServe: Double, pointPerfVarianceOnReturn: Double)