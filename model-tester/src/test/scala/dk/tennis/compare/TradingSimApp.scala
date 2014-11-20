package dk.tennis.compare

import dk.tennis.compare.trading.sim.tradingSim
import dk.bayes.math.gaussian.Gaussian
import scala.math._

object TradingSimApp extends App {

  val summary = tradingSim(
    trueProb = Gaussian(0.5, pow(0.2, 2)),
    exProbBias = 0.02, exProbVar = pow(0.05, 2),
    modelProbVar = pow(0.1, 2),
    betMinMargin = 0.1, betsNum = 1000, commission = 0.05)
  println("profit=%.2f, stake=%.2f, margin=%.2f".format(summary.profit, summary.stake, summary.profit / summary.stake))
}