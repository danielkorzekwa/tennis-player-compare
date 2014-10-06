package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.opponent.PlayerSkill

/**
 * @param priorSkillsGivenOpponent  key - opponent name, value - player skills against opponent
 */
case class SkillsModelParams(skillPriorMeanOnServe: Double, skillPriorMeanOnReturn: Double,
  skillCovParams: Array[Double],
  priorSkillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], priorSkillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]])