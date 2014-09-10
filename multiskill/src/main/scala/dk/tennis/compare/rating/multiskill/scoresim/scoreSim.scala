package dk.tennis.compare.rating.multiskill.scoresim

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import scala.math._
import scala.util.Random
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp._
import dk.tennis.compare.rating.multiskill.model.perfdiff._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor._
import dk.tennis.compare.rating.multiskill.model.perfdiff.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShort
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov._

/**
 * Simulates scores from a history of tennis matches
 */
object scoreSim {

  /**
   * Returns scores with simulated points won by both players.
   *
   */
  def apply(scores: Array[Score], meanFunc: Player => Double, playerCovFunc: PlayerCovFunc, logPerfStdDev: Double): Array[SimScore] = {

    val rand = new Random(0)

    val gameSkills: Seq[MultivariateGaussian] = MultiGPSkillsFactor3(meanFunc, playerCovFunc, Score.toPlayers(scores)).sampleGameSkills(rand)
    val gamePerfDiffs: Seq[Gaussian] = gameSkillsToPerfDiffs(gameSkills, logPerfStdDev)

    val simulScores = scores.zip(gameSkills).map {
      case (score, gameSkills) =>
        val gamePerfDiff = gameSkillToPerfDiff(gameSkills, logPerfStdDev)
        val simulScore = simScore(score, gamePerfDiff, rand)
        SimScore(gameSkills, gamePerfDiff, simulScore)
    }
    simulScores
  }

  def simScore(score: Score, gamePerfDiff: Gaussian, rand: Random): Score = {

    val player1PointProb = exp(OutcomeLik.loglik(gamePerfDiff, true))
    var player1PointsWon = 0
    var player2PointsWon = 0
    for (i <- 1 to (score.pointsWon.get._1 + score.pointsWon.get._2)) {
      if (rand.nextDouble < player1PointProb) player1PointsWon += 1
      else player2PointsWon += 1
    }
    val simulScore = score.copy(pointsWon = Some(player1PointsWon,player2PointsWon))

    simulScore
  }
}