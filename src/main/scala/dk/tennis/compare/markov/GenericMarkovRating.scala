package dk.tennis.compare.markov

import MarkovRating._
import scala.Math._
import scala.annotation.tailrec
import java.util.Date

/**
 * Calculates tennis player rating based on Markov localisation.
 *
 * @param startRatingValue
 * @param endRatingValue
 * @param calculateWinProbability Function, that calculates win probability on serve by player A against player B. (playerARatingOnServe, playerBRatingOReturn) => winProbability.
 */
class GenericMarkovRating(startRatingValue: Int, endRatingValue: Int,calculateWinProbOnServe: (Int, Int) => Double) extends MarkovRating {

  /**Calculates new ratings for players A and B given the evidence (number of points won/lost on serve by player A against player B in a tennis match).
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

  /**Calculates point on serve winning probability by player A against player B.
   * @see MarkovRating.calcWinProb()*/
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

    val (newRatingAOnServe,newRatingBOnReturn) = calcRatings(currRatingA.ratingOnServe, currRatingB.ratingOnReturn,result.playerAPointsWon,result.playerAPointsLost)
    
    val newRatingA = currRatingA.copy(ratingOnServe = newRatingAOnServe)
    val newRatingB = currRatingB.copy(ratingOnReturn = newRatingBOnReturn)

    val newRatings = ratings + (result.playerA -> newRatingA, result.playerB -> newRatingB)
    newRatings
  }

  /**Tuple2[rating on serve, rating on return]*/
  private def initRating(timestamp: Date) = PlayerRating(
    Rating.generateRatings(startRatingValue,endRatingValue),
    Rating.generateRatings(startRatingValue,endRatingValue))

}