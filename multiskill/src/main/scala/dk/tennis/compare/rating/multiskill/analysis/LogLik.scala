package dk.tennis.compare.rating.multiskill.analysis

import scala.math._
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel

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

  /**
   *  Returns [total,avg,totalPoints] log likelihood]
   */
  def logLik(skillsMarginal: Seq[Gaussian], players: Seq[Player]): Tuple3[Double, Double, Double] = {
    val pointProbsOnServe: Seq[Double] = toPointProbs(skillsMarginal)

    //Tuple2(pointsWon,pointsTotal)
    val pointStatsOnServe: Seq[Tuple2[Int, Int]] = toPointStatsOnServe(players)

    //(pointProb,pointsWon,pointsTotal)
    val predictions: Seq[Tuple3[Double, Int, Int]] = pointProbsOnServe.zip(pointStatsOnServe).map { case (prob, (pointsWon, pointsTotal)) => (prob, pointsWon, pointsTotal) }

    logLik(predictions)
  }

  private def toPointProbs(skillMarginals: Seq[Gaussian]): Seq[Double] = {
    val (pointPerfVarianceOnServe, pointPerfVarianceOnReturn) = (195.61914136268837, 155)

    val pointModel = GenericPointModel(pointPerfVarianceOnServe, pointPerfVarianceOnReturn)
    //val pointProbs = skillMarginals.grouped(2).map { case Seq(skill1, skill2) => pointModel.pointProb(initialSkillsOnServe, initialSkillsOnReturn) }
    val pointProbs = skillMarginals.grouped(2).map { case Seq(skill1, skill2) => pointModel.pointProb(skill1, skill2) }
    pointProbs.toList
  }

  private def toPointStatsOnServe(players: Seq[Player]): Seq[Tuple2[Int, Int]] = {

    val pointStatsOnServe = players.grouped(2).map {
      case Seq(pOnServe, pOnReturn) =>
        (pOnServe.pointsWon, pOnServe.pointsWon + pOnReturn.pointsWon)
    }

    pointStatsOnServe.toList
  }
}