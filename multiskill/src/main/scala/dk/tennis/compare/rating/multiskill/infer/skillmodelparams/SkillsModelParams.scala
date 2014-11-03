package dk.tennis.compare.rating.multiskill.infer.skillmodelparams
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc

/**
 * @param priorSkillsGivenOpponent  key - opponent name, value - player skills against opponent
 */
case class SkillsModelParams(skillMeanFunc: (Player) => Double, skillCovFunc: CovFunc)