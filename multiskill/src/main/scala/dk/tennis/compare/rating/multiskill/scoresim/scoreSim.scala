package dk.tennis.compare.rating.multiskill.scoresim

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import scala.math._
import scala.util.Random
import dk.tennis.compare.rating.multiskill.model.perfdiff._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor._
import dk.tennis.compare.rating.multiskill.model.perfdiff.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.factorops._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov._

/**
 * Simulates scores from a history of tennis matches
 */
object scoreSim {

  /**
   * Returns scores with simulated points won by both players.
   *
   */
  def apply(scores: Array[Score], meanFunc: Player => Double, playerCovFunc: CovFunc, logPerfStdDev: Double,randSeed:Int): Array[SimScore] = {

    val rand = new Random(randSeed)

    val gameSkills: Seq[MultivariateGaussian] = sampleGameSkills(Score.toPlayers(scores),meanFunc, playerCovFunc,rand)
    val gamePerfDiffs: Seq[Gaussian] = gameSkillsToPerfDiffs(gameSkills, logPerfStdDev).map(p => p.perfDiff)

    val simulScores = scores.zip(gameSkills).map {
      case (score, gameSkills) =>
        val gamePerfDiff = gameSkillToPerfDiff(gameSkills, logPerfStdDev)
        val simulScore = simScore(score, gamePerfDiff, rand)
        SimScore(gameSkills, gamePerfDiff, simulScore)
    }
    simulScores
  }

  def simScore(score: Score, gamePerfDiff: PerfDiff, rand: Random): Score = {

    val player1PointProb = exp(OutcomeLik.loglik(gamePerfDiff.perfDiff, true))
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