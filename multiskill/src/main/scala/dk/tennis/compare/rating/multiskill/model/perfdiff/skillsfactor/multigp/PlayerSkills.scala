package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.gaussian.Gaussian

case class PlayerSkills(skillsGaussian: MultivariateGaussian, players: Array[Player]) {

  def marginal(player: Player): Gaussian = {
    val playerIndex = players.indexOf(player)
    
    skillsGaussian.marginal(playerIndex)
   
  }

}