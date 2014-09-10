package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShort
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.cov.PlayerCovFuncShortLong
import scala.math._
import java.util.Date
import dk.bayes.math.gaussian.MultivariateGaussian
import breeze.plot.Figure
import breeze.plot._

object PlayerCovPlotting extends App {

  val sampledSkills = sampleSkills()

  val f = Figure()
  val p = f.subplot(0)
  p += plot((1 to sampledSkills.size).map(i => i.toDouble), sampledSkills)
  p.ylim(0, 10)
  System.in.read()

  private def sampleSkills(): Array[Double] = {

    val covFunc = PlayerCovFuncShortLong(Array(-1.0208097533488787, 3.779464609040853, 0.08623887840621325, 8.158528184446062))
   // val covFunc = PlayerCovFuncShort(Array(log(1), log(300)))
    //val covFunc = PlayerCovFuncShortLong(Array(log(0.2), log(10), log(1), log(300)))

    val mean = Matrix((1 to 365).map(i => 5d).toArray)
    val players = (1 to 365).map { i =>
      Player("playerName", "opponent", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i))

    }.toArray

    //val playerSkills
    val variance = covFunc.covarianceMatrix(players)

    val sampledSkills = MultivariateGaussian(mean, variance).draw

    sampledSkills
  }
}