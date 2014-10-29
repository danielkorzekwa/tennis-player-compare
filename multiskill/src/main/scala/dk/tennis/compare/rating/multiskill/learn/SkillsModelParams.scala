package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.SkillsGivenOpponent


/**
 * @param priorSkillsGivenOpponent  key - opponent name, value - player skills against opponent
 */
case class SkillsModelParams( skillMeanFunc: (Player) => Double,
  skillCovParams: Array[Double],
 skillsGivenOpponent:SkillsGivenOpponent)