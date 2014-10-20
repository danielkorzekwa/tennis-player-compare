package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill


/**
 * @param priorSkillsGivenOpponent  key - opponent name, value - player skills against opponent
 */
case class SkillsModelParams(skillPriorMeanOnServe: Double, skillPriorMeanOnReturn: Double,
  skillCovParams: Array[Double],
 skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]])