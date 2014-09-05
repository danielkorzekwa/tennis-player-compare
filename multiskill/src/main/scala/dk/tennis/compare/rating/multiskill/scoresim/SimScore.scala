package dk.tennis.compare.rating.multiskill.scoresim

import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.Gaussian

case class SimScore(gameSkills: MultivariateGaussian, gamePerfDiff: Gaussian, score: Score) {
  def playerSkill(playerName: String): Gaussian = {
    
    if (score.player1.playerName.equals(playerName)) gameSkills.marginal(0)
    else if (score.player2.playerName.equals(playerName)) gameSkills.marginal(1)
    else throw new IllegalArgumentException("Player not found:" + playerName)
  }
}