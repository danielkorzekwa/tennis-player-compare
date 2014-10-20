package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype

/**
 * If false then player is defensive
 */
case class OpponentType(player: String, offensive: Boolean) {

  def offensiveBit(): Int = if (offensive) 1 else 0
  def defensiveBit: Int = if (!offensive) 1 else 0
}