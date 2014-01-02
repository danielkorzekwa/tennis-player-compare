package dk.tennis.compare.rating.multiskill.analysis

object Calibrate {

  /**
   * @param predictions Seq of [predicted prob, points won, points total]
   * @param binsNum Number of bins
   *
   * @return Seq of [avg predicted prob, avg actual prob, num of samples]
   */
  def calibrate(predictions: Seq[Tuple3[Double, Int, Int]], binsNum: Int): Seq[Tuple3[Double, Double, Int]] = {

    val predictionsByProb = predictions.groupBy { p => (p._1 * binsNum).toInt }.values.map { values =>
      val sampleSize = values.size

      val (predictedProbSum, pointsWonSum, pointsTotalSum) = sum(values)
      Tuple3(predictedProbSum / sampleSize, pointsWonSum.toDouble / pointsTotalSum, sampleSize)
    }

    predictionsByProb.toSeq

  }

  private def sum(tuples: Seq[Tuple2[Int, Int]]): Tuple2[Int, Int] = {
    tuples.reduceLeft((sum, t) => (sum._1 + t._1, sum._2 + t._2))
  }

  private def sum(tuples: Seq[Tuple3[Double, Int, Int]]): Tuple3[Double, Int, Int] = {
    tuples.reduceLeft((sum, t) => (sum._1 + t._1, sum._2 + t._2, sum._3 + t._3))
  }
}