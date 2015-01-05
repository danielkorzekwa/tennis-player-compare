package dk.tennis.compare.rating.multiskill.infer.skills

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

trait InferSkill {

   def inferSkill(player: Player): PlayerSkill
}