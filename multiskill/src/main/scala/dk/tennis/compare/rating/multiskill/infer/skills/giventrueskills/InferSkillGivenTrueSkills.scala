package dk.tennis.compare.rating.multiskill.infer.skills.giventrueskills

import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.Gaussian

case class InferSkillGivenTrueSkills(trueSkills: Map[Player, Gaussian]) extends InferSkill {

  def inferSkill(player: Player): PlayerSkill = {

    PlayerSkill(trueSkills(player), player)

  }
}