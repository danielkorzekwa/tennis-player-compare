package dk.tennis.compare.trading.sim

import dk.bayes.math.gaussian.Gaussian
import scala.util.Random
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.ThreadLocalRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.math._

object tradingSim {

  def apply(trueProb: Gaussian, exProbBias: Double, exProbVar: Double, modelProbVar: Double,
    betMinMargin: Double, betsNum: Int, commission: Double): TradingSimSummary = {

    val rand = new Random()
    var profit = 0d
    var stake = 0d

    (1 to betsNum).foreach { i =>

      val trueProbVal = trueProb.draw()
      val exProb = Gaussian(trueProbVal + exProbBias, exProbVar).draw
      val modelProb = Gaussian(trueProbVal, modelProbVar).draw

      if (checkProbs(trueProbVal, exProb, modelProb) && (1d / exProb) * modelProb > (1 + betMinMargin)) {

        profit += trueProbVal * (1 - commission) * (1d / exProb - 1) + (1 - trueProbVal) * (-1)
        stake += 1
      }

    }
    TradingSimSummary(profit, stake)
  }

  private def checkProbs(trueProb: Double, exProb: Double, modelProb: Double): Boolean = {
    trueProb > 0.001 && trueProb < 0.99 && exProb > 0.001 && exProb < 0.99 && modelProb > 0.001 && modelProb < 0.99
  }
}