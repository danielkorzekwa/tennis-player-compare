package dk.tennis.compare.rating.multiskill.model.tournament

import org.junit._
import Assert._

class GenericTournamentModelTest {

  @Test def best_of_two {

    val draw = List(
      ("p1", "p2"))

    def winProb(player1: String, player2: String): Double = 0.6

    val tProbs = GenericTournamentModel.winningProbs(draw, winProb)
    val expectedProbs = Map(("p1", 0.6), ("p2", 0.4))
    assertEquals(expectedProbs, tProbs)

  }

  @Test def best_of_four {
    val draw = List(
      ("p1", "p2"), ("p3", "p4"))

    def winProb(player1: String, player2: String): Double = (player1, player2) match {
      case ("p1", "p2") => 0.6
      case ("p3", "p4") => 0.64
      case ("p1", "p3") => 0.62
      case ("p1", "p4") => 0.74
      case ("p2", "p3") => 0.66
      case ("p2", "p4") => 0.61
    }

    val tProbs = GenericTournamentModel.winningProbs(draw, winProb)
    assertEquals(4, tProbs.size)
    assertEquals(0.3979, tProbs("p1"), 0.0001)
    assertEquals(0.2568, tProbs("p2"), 0.0001)
    assertEquals(0.23296, tProbs("p3"), 0.0001)
    assertEquals(0.11232, tProbs("p4"), 0.0001)
  }
}