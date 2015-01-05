package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.bayes.math.linear.Matrix
import java.util.Date
import dk.bayes.math.gaussian.MultivariateGaussian
import scala.util.Random
import breeze.plot.Figure
import breeze.plot._
import scala.collection.immutable.TreeMap

object plotSimPlayerSkillsOverTime {

  def apply(players: Seq[Player], skillMeanFunc: (Player) => Double, skillCovFunc: CovFunc) {

    val sampledSkills = sampleSkills(players, skillMeanFunc, skillCovFunc)

    val playerSkills: Seq[Array[Double]] = sampledSkills.grouped(sampledSkills.size / players.size).toList

    val f = Figure()
    f.subplot(0).legend = true
    f.subplot(0).ylim(0, 10)

    players.zipWithIndex.foreach {
      case (player, index) =>

        val skills = playerSkills(index)
        f.subplot(0) += plot((1 to skills.size).map(i => i.toDouble), skills, name = player.toString)

    }

  }

  private def sampleSkills(players: Seq[Player], skillMeanFunc: (Player) => Double, skillCovFunc: CovFunc): Array[Double] = {

    val meanArray = players.flatMap(p => Array.fill(365)(skillMeanFunc(p))).toArray
    val mean = Matrix(meanArray)

    val allPlayers = players.flatMap { p =>
      (1 to 365).map { i => p.copy(timestamp = new Date(1000L * 3600 * 24 * i)) }
    }.toArray

    val variance = skillCovFunc.covarianceMatrix(allPlayers)
    val sampledSkills = MultivariateGaussian(mean, variance).draw(randSeed = new Random().nextInt)

    sampledSkills
  }
}