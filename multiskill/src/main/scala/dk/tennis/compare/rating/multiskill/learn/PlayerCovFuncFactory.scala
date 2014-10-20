package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc


/**
 * @param skillsGivenOpponent key - opponent name, value - player skills against opponent
 */
trait PlayerCovFuncFactory {

  def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc
}