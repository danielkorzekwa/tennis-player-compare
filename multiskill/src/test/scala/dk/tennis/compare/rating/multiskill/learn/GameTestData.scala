package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import scala.util.Random
import scala.math._

object GameTestData {

  private val DAY_MILLIS = 1000L * 3600 * 24

  val players: Array[Player] = Array(
    Player("p1", "p2", true, new Date(DAY_MILLIS * 1)), Player("p2", "p1", false, new Date(DAY_MILLIS * 1)),
    Player("p1", "p2", true, new Date(DAY_MILLIS * 2)), Player("p2", "p1", false, new Date(DAY_MILLIS * 2)),
    Player("p1", "p2", true, new Date(DAY_MILLIS * 3)), Player("p2", "p1", false, new Date(DAY_MILLIS * 3)),
    Player("p1", "p2", true, new Date(DAY_MILLIS * 4)), Player("p2", "p1", false, new Date(DAY_MILLIS * 4)),
    Player("p1", "p2", true, new Date(DAY_MILLIS * 5)), Player("p2", "p1", false, new Date(DAY_MILLIS * 5)))

  val scores: Array[Score] = Array(
    Score(Player("p1", "p2", true, new Date(DAY_MILLIS * 1)), Player("p2", "p1", false, new Date(DAY_MILLIS * 1)), 90, 10),
    Score(Player("p1", "p2", true, new Date(DAY_MILLIS * 2)), Player("p2", "p1", false, new Date(DAY_MILLIS * 2)), 80, 20),
    Score(Player("p1", "p2", true, new Date(DAY_MILLIS * 3)), Player("p2", "p1", false, new Date(DAY_MILLIS * 3)), 70, 30),
    Score(Player("p1", "p2", true, new Date(DAY_MILLIS * 4)), Player("p2", "p1", false, new Date(DAY_MILLIS * 4)), 60, 40),
    Score(Player("p1", "p2", true, new Date(DAY_MILLIS * 5)), Player("p2", "p1", false, new Date(DAY_MILLIS * 5)), 50, 50))

//  def getScores(playersNum: Int): Array[Score] = {
//
//    val rand = new Random(5656456)
//
//    val playerIds = (1 to playersNum)
//    val games = playerIds.combinations(2)
//
//    val scores = games.zipWithIndex.flatMap {
//      case (Seq(p1, p2), index) =>
//        Array(Score(50 + rand.nextInt(2 * p1 * (2 * index + 1)), 50 + rand.nextInt(2 * p2 * (2 * index + 1))),
//          Score(50 + rand.nextInt(2 * p2 * (2 * index + 1)), 50 + rand.nextInt(2 * p1 * (2 * index + 1))))
//    }
//
//    scores.toArray
//
//  }
//  def getGamePlayers(playersNum: Int): Array[Player] = {
//
//    val playerIds = (1 to playersNum)
//    val games = playerIds.combinations(2)
//
//    val players = games.zipWithIndex.flatMap {
//      case (Seq(p1, p2), index) =>
//        Array(Player(p1.toString, p2.toString, true, new Date(index)), Player(p2.toString, p1.toString, false, new Date(index)),
//          Player(p2.toString, p1.toString, true, new Date(index)), Player(p1.toString, p2.toString, false, new Date(index)))
//    }
//
//    players.toArray
//
//  }

}