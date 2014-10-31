package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import scala.math._
import java.util.Date
import dk.bayes.math.gaussian.MultivariateGaussian
import breeze.plot.Figure
import breeze.plot._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeOverTimeCovFunc
import scala.util.Random

object PlayerCovPlotting extends App {

  val sampledSkills = sampleSkills()
  val f = Figure()
  val p1 = f.subplot(0)
  p1 += plot((1 to sampledSkills.size / 2).map(i => i.toDouble), sampledSkills.take(sampledSkills.size / 2))
  p1.ylim(0, 10)

  val p2 = f.subplot(0)
  p2 += plot((1 to sampledSkills.size / 2).map(i => i.toDouble), sampledSkills.takeRight(sampledSkills.size / 2))
  p2.ylim(0, 10)

  System.in.read()

  private def sampleSkills(): Array[Double] = {

    val opponentTypeMap = Map(
      "opponent1" -> OpponentType("opponent1", true),
      "opponent2" -> OpponentType("opponent1", false))

//    val covFunc = OpponentTypeOverTimeCovFunc(
//      Array(log(1), log(0.4), log(0.6),
//        log(0.3), log(30), log(1), log(365)),
//      opponentTypeMap)
      
        val covFunc = OpponentTypeOverTimeCovFunc(
      Array(log(1), log(0.4), log(0.6),
        -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913),
      opponentTypeMap)

      
    //  -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3
    // val covFunc = PlayerCovFuncShort(Array(log(1), log(300)))
    //val covFunc = PlayerCovFuncShortLong(Array(log(0.2), log(10), log(1), log(300)))

    val mean = Matrix((1 to 2 * 365).map(i => 5d).toArray)
    val players1 = (1 to 365).map { i =>
      Player("playerName", "opponent1", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i))

    }.toArray
    val players2 = (1 to 365).map { i =>
      Player("playerName", "opponent2", onServe = true, timestamp = new Date(1000L * 3600 * 24 * i))

    }.toArray

    //val playerSkills
    val variance = covFunc.covarianceMatrix(players1 ++ players2)

    val sampledSkills = MultivariateGaussian(mean, variance).draw(randSeed=new Random().nextInt)

    sampledSkills
  }
}