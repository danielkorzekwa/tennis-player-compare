package dk.tennis.compare.markov

import MarkovRating._

import scala.annotation.tailrec
import java.util.Date

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

/**
 * Calculates tennis player rating based on Markov localisation.
 *
 * @param startRatingValue
 * @param endRatingValue
 * @param calculateWinProbability Function, that calculates win probability on serve by player A against player B. (playerARatingOnServe, playerBRatingOReturn) => winProbability.
 */
class GenericMarkovRating(startRatingValue: Int, endRatingValue: Int, calculateWinProbOnServe: (Int, Int) => Double) extends MarkovRating {

   private var ratings = Map[String, PlayerRating]()
  
  /**
   * Calculates new ratings for players A and B given the evidence (number of points won/lost on serve by player A against player B in a tennis match).
   * @see MarkovRating.calcRatings()
   */
  def calcRatings(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating],
    playerAPointsWon: Int, playerAPointsLost: Int): Tuple2[List[Rating], List[Rating]] = {

    val playerAMarginals = playerARatingOnServe.map(r => Rating(r.ratingValue, 0)).toArray
    val playerBMarginals = playerBRatingOnReturn.map(r => Rating(r.ratingValue, 0)).toArray
    var totalFactorSum = 0d

    for (
      (ratingA, indexA) <- playerARatingOnServe.view.zipWithIndex;
      (ratingB, indexB) <- playerBRatingOnReturn.view.zipWithIndex
    ) {

      val winOnServeProb = calculateWinProbOnServe(ratingA.ratingValue, ratingB.ratingValue)

      val playersABFactor = ratingA.ratingProb * ratingB.ratingProb * pow(winOnServeProb, playerAPointsWon) * pow((1 - winOnServeProb), playerAPointsLost)

      playerAMarginals(indexA) = playerAMarginals(indexA).copy(ratingProb = playerAMarginals(indexA).ratingProb + playersABFactor)
      playerBMarginals(indexB) = playerBMarginals(indexB).copy(ratingProb = playerBMarginals(indexB).ratingProb + playersABFactor)

      totalFactorSum += playersABFactor
    }

    val normPlayerAMarginals = playerAMarginals.map(r => r.copy(ratingProb = r.ratingProb / totalFactorSum))
    val normPlayerBMarginals = playerBMarginals.map(r => r.copy(ratingProb = r.ratingProb / totalFactorSum))

    Tuple2(normPlayerAMarginals.toList, normPlayerBMarginals.toList)
  }

  /**
   * Calculates point on serve winning probability by player A against player B.
   * @see MarkovRating.calcWinProb()
   */
  def calcWinProb(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating]): Double = {

    var factorWonSum = 0d
    var factorLossSum = 0d

    for (playerARating <- playerARatingOnServe; playerBRating <- playerBRatingOnReturn) {
      val winOnServeProb = calculateWinProbOnServe(playerARating.ratingValue, playerBRating.ratingValue)

      val factorWon = playerARating.ratingProb * playerBRating.ratingProb * winOnServeProb
      val factorLoss = playerARating.ratingProb * playerBRating.ratingProb * (1 - winOnServeProb)

      factorWonSum += factorWon
      factorLossSum += factorLoss
    }
    factorWonSum / (factorWonSum + factorLossSum)

  }

  /**
   * Calculates tennis player ratings based on a history of tennis results
   * @return Map[playerName, playerRating]
   */
  def calcPlayerRatings(results: List[Result]): Map[String, PlayerRating] = {
    /**Map[player,Rating*/
    val ratings = results.foldLeft(Map[String, PlayerRating]())((currentRatings, result) => updateRatings(currentRatings, result))
    ratings
  }

  private def updateRatings(ratings: Map[String, PlayerRating], result: Result): Map[String, PlayerRating] = {

    val currRatingA = ratings.getOrElse(result.playerA, initRating(result.timestamp))
    val currRatingB = ratings.getOrElse(result.playerB, initRating(result.timestamp))

    val (newRatingAOnServe, newRatingBOnReturn) = calcRatings(currRatingA.ratingOnServe, currRatingB.ratingOnReturn, result.playerAPointsWon, result.playerAPointsLost)

    val newRatingA = currRatingA.copy(ratingOnServe = newRatingAOnServe)
    val newRatingB = currRatingB.copy(ratingOnReturn = newRatingBOnReturn)

    val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)

//For analysis only - plotting player ratings
//    if (result.playerA.equals("Roger Federer") && ratings.isDefinedAt("Roger Federer") && ratings.isDefinedAt("Andy Roddick")) {
//      val probs = newRatings("Andy Roddick").ratingOnServe.map(r => if (r.ratingProb > 0.1) (r.ratingProb * 1000).toInt else 1)
//      println(result.timestamp + ":" + calcWinProb(ratings("Roger Federer").ratingOnServe, ratings("Andy Roddick").ratingOnReturn))
//      val ratingHistVec = DenseVector.zeros[Double](probs.sum)
//
//      var position: Int = 0
//      for ((e, index) <- probs.zipWithIndex) {
//        ratingHistVec(position to (position + e - 1)) := index.toDouble + 1
//        position += e
//      }
//      hist(ratingHistVec, 100)
//    }

    newRatings
  }
  
   /**Processes tennis match event and updates internal player ratings.
   * 
   */
  def sendResult(result:Result) {
     ratings = updateRatings(ratings, result)
  }
  
  /**
   * @return Map [player name, player rating],
   */
  def getRatings(): Map[String,PlayerRating] = ratings
  
  /**Tuple2[rating on serve, rating on return]*/
  private def initRating(timestamp: Date) = PlayerRating(
    Rating.generateRatings(startRatingValue, endRatingValue),
    Rating.generateRatings(startRatingValue, endRatingValue))

}