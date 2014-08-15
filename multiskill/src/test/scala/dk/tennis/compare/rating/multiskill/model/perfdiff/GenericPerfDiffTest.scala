package dk.tennis.compare.rating.multiskill.model.perfdiff

import scala.math.log
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SingleGPSkillsFactor
import scala.io.Source
import scala.collection.immutable.HashSet
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.MultiGPSkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph

class GenericPerfDiffTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"

  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2006, 2011)

  //val playersList = HashSet("Roger Federer", "Rafael Nadal", "Novak Djokovic","Gael Monfils")
  val playersList = HashSet[String]()
  val players: Array[Player] = Player.toPlayers(tournaments, playersList)
  val scores: Array[Score] = Score.toScores(tournaments, playersList)
  val playerNames: Array[String] = Player.toPlayers(tournaments, playersList).map(p => p.playerName).distinct

  logger.info(s"Players by name: ${playerNames.size}")
  logger.info(s"All players in all games: ${players.size}")
  logger.info(s"All games (on serve + on return): ${scores.size}")

  val (logSf, logEll, logPerfStdDev) = (-0.5157492948837095, 2.311460778424053, 2.3294118372478136)

  @Test def test {

    val skillsFactor = MultiGPSkillsFactor(logSf, logEll, playerSkillMeanPrior, players)
    val skillsFactorGraph = SkillsFactorGraph(scores, logPerfStdDev, skillsFactor)
    //  println( skillsFactor.getPriorSkillsByPlayersMap()("Roger Federer").v)

    val infer = GenericPerfDiff(skillsFactorGraph, logPerfStdDev, scores, threshold = 0.5)

    logger.info("Calculating log likelihood")

    val perfDiffs = infer.inferPerfDiffs()

    val loglik = OutcomeLik.totalLoglik(perfDiffs, scores)

    println("Total/avg log lik: %.3f/%.3f".format(loglik, loglik / scores.map(s => s.p1PointsWon + s.p2PointsWon).sum))

    val p1 = "Roger Federer"
    val p2 = "Radek Stepanek"
    scores.zip(perfDiffs).zipWithIndex.foreach {
      case ((score, perfDiff), index) =>

        var matched = false
        if (score.player1.playerName.equals(p1) && score.player2.playerName.equals(p2)) {
          println(score.player1 + ":" + exp(OutcomeLik.loglik(perfDiff, true)))
          matched = true
        }

        if (score.player1.playerName.equals(p2) && score.player2.playerName.equals(p1)) {
          println(score.player1 + ":" + exp(OutcomeLik.loglik(perfDiff, true)))
          matched = true
        }

        if ((index + 1) % 2 == 0 && matched)
          println("-------------------------")
    }

  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) 4.41 else -0.59
  }

}