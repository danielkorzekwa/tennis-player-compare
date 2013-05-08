package dk.tennis.compare.matching.playerspair

/**
 * Matches players pairs together.
 *
 * @author Daniel Korzekwa
 */
trait PlayersPairMatcher {

  /**
   * Matches players pairs together.
   *
   * @param playersPair1 Tuple2(player1Name,player2Name)
   * @param playersPair2 Tuple2(player1Name,player2Name)
   *
   *  @return True if players pairs are the same. Players order in a player pair and lower/upper case of player names are ignored.
   */
  def matchPlayersPair(playersPair1: Tuple2[String, String], playersPair2: Tuple2[String, String]): Boolean
}