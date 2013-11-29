package dk.tennis.compare.matching.playerspair

/**
 * Default implementation of the PlayersPairMatcher
 *
 * @author Daniel Korzekwa
 */
object GenericPlayersPairMatcher extends PlayersPairMatcher {

  def matchPlayersPair(playersPair1: Tuple2[String, String], playersPair2: Tuple2[String, String]): Boolean = {

    val normalisedPair1 = normalise(List(playersPair1._1, playersPair1._2))

    val normalisedPair2 = normalise(List(playersPair2._1, playersPair2._2))
    List(playersPair1._1, playersPair1._2)

    normalisedPair1.equals(normalisedPair2)
  }

  private def normalise(playerNames: List[String]): List[String] = {
    playerNames.map(playerName => playerName.toLowerCase).sorted
  }
}