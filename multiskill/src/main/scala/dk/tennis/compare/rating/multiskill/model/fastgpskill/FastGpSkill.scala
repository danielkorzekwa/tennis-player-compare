package dk.tennis.compare.rating.multiskill.model.fastgpskill

import dk.bayes.math.gaussian.Gaussian

trait FastGpSkill {

  def skillMarginals(players:Array[Player]):Seq[Gaussian]
}