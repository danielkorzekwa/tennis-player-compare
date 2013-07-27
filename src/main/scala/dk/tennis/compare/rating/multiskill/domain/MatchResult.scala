package dk.tennis.compare.rating.multiskill.domain

case class MatchResult(player1: String, player2: String, pointResults: Seq[PointResult],
  player1Won: Boolean, numOfSets: Int) {

  require(numOfSets == 2 || numOfSets == 3, "The number of sets can be 2 or 3, but is " + numOfSets)
}
    
   