package dk.tennis.compare.rating.multiskill.domain

case class PlayerSkill(mean: Double, variance: Double) {

  def transition(transitionVariance: Double) = {
    this.copy(variance = variance + transitionVariance)
  }
}