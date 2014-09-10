package dk.tennis.compare.trading

object tradingSim {

  /**
   * @param outcomes 0 - lose, 1 -win
   *
   * @return Profit
   */
  def apply(outcomes: Seq[Outcome]): Double = {

    var totalProfit = 0d

    val betProfits = outcomes.filter(o => o.price.isDefined).foreach { outcome =>
      val price = outcome.price.get
      val trueProb = outcome.trueProb
      val outcomeBit = if (outcome.win) 1 else 0
      val stake = 1

      val betProfit =
        if (1d / outcome.trueProb < price) stake * price * outcomeBit - stake //back bet 
        else -stake * price * outcomeBit - (-stake) //lay bet

      totalProfit += betProfit
      println(totalProfit)

    }

    totalProfit
  }
}