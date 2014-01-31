package dk.tennis.compare.rating.multiskill.matchloader

case class MatchResult(player1: String, player2: String, player1Won: Boolean, numOfSets: Int, p1Stats: PlayerStats, p2Stats: PlayerStats) {

  require(numOfSets == 2 || numOfSets == 3, "The number of sets can be 2 or 3, but is " + numOfSets)

  def containsPlayer(playerName: String): Boolean = player1.equals(playerName) || player2.equals(playerName)
}
    
   