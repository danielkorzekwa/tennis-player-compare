package dk.tennis.compare.rating.multiskill.infer.skillmodelparams

import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.math._
import java.util.Date
import dk.bayes.math.gaussian.MultivariateGaussian
import breeze.plot.Figure
import breeze.plot._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import scala.util.Random
import scala.Array.canBuildFrom
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface.SurfaceCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.surface.SurfaceCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponentseiso.OpponentSeIsoCovFunc
import scala.math._

object PlayerCovPlotting extends App {

  val sampledSkills = sampleSkills()

  val f = Figure()
  
  val List(hardSkills,claySkills,grassSkills) = sampledSkills.grouped(sampledSkills.size / 3).toList
  
  f.subplot(0) += plot((1 to sampledSkills.size / 3).map(i => i.toDouble), hardSkills)
  f.subplot(0).ylim(0, 10)

  f.subplot(0) += plot((1 to sampledSkills.size / 3).map(i => i.toDouble), claySkills)
  f.subplot(0).ylim(0, 10)
  
   f.subplot(0) += plot((1 to sampledSkills.size / 3).map(i => i.toDouble), grassSkills)
  f.subplot(0).ylim(0, 10)

  System.in.read()

  
  
  
  private def sampleSkills(): Array[Double] = {

   // val covFunc = OpponentSeIsoCovFunc(Array(log(1000)))
    val covFunc = SurfaceCovFunc(Array( log(1), log(20),log(20)))

    val mean = Matrix((1 to 3 * 365).map(i => 5d).toArray)

    val players1 = (1 to 365).map { i =>
      Player("playerName", "opponent1", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i), Surface.HARD)
    }.toArray

    val players2 = (1 to 365).map { i =>
      Player("playerName", "opponent2", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i), Surface.CLAY)
    }.toArray

    val players3 = (1 to 365).map { i =>
      Player("playerName", "opponent3", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i), Surface.GRASS)
    }.toArray

    val variance = covFunc.covarianceMatrix(players1 ++ players2 ++ players3)
    val sampledSkills = MultivariateGaussian(mean, variance).draw(randSeed = new Random().nextInt)

    sampledSkills
  }
}