package dk.tennis.compare.rating.multiskill.model.fastgpskill

import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player

trait FastGpSkill {

  def skillMarginals(players:Array[Player]):Seq[Gaussian]
}