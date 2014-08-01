package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score

object GameTestData {

  val players:Array[Player] = Array(
    Player("p1", "p2", true, new Date(1)), Player("p2", "p1", false, new Date(1)),
    Player("p1", "p2", true, new Date(2)), Player("p2", "p1", false, new Date(2)),
    Player("p1", "p2", true, new Date(3)), Player("p2", "p1", false, new Date(3)),
    Player("p1", "p2", true, new Date(4)), Player("p2", "p1", false, new Date(4)),
    Player("p1", "p2", true, new Date(5)), Player("p2", "p1", false, new Date(5)))

  val scores:Array[Score] = Array(
    Score(90, 10), Score(80, 20), Score(70, 30), Score(60, 40), Score(50, 50))

}