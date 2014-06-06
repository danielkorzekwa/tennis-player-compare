package dk.tennis.compare.rating.multiskill.analysis

import scala.math._
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player

object LogLik {

  /**
   * @param predictions Seq of [predicted prob, points won, points total]
   *
   * Returns [total,avg,totalPoints] log likelihood
   */
  def logLik(predictions: Seq[Tuple3[Double, Int, Int]]): Tuple3[Double, Double, Double] = {

    val totalLoglik = predictions.foldLeft(0d) { (totalLoglik, p) =>

      val (pointProb, pointsWon, pointsTotal) = p
      val logLik = pointsWon * log(pointProb) + (pointsTotal - pointsWon) * log1p(-pointProb)
      totalLoglik + logLik
    }

    val totalPoints = predictions.map(p => p._3).sum
    val avgLogLik = totalLoglik / totalPoints

    (totalLoglik, avgLogLik, totalPoints)
  }

}