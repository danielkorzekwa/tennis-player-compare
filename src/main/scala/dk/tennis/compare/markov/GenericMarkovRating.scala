package dk.tennis.compare.markov

import MarkovRating._
import scala.Math._
import scala.annotation.tailrec

/**
 * Calculates tennis player rating based on Markov localisation.
 *
 */
object GenericMarkovRating extends MarkovRating {

  /**
   * Calculates new ratings for players A and B given the evidence (number of points won/lost on serve by player A against player B in a tennis match).
   *
   * @param playerARatingOnServe List of rating values and probabilities representing player A rating on serve.
   *
   * @param playerBRatingOnServe List of rating values and probabilities representing player B rating on return.
   *
   * @param playerAWonPoints/playerAlostPoints Number of points won/lost by player A against player B.
   *
   * @param calculateWinProbability Function, that calculates win probability on serve by player A against player B. (playerARatingOnServe, playerBRatingOnReturn) => winProbability.
   *
   * @return Tuple2[New rating on serve for player A, New rating on return for player B]
   */
  def calcRatings(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating],
    playerAPointsWon: Int, playerAPointsLost: Int, calculateWinProbOnServe: (Int, Int) => Double): Tuple2[List[Rating], List[Rating]] = {

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

  /**Calculates point on serve winning probability by player A against player B.*/
  def calcWinProb(playerARatingOnServe: List[Rating], playerBRatingOnReturn: List[Rating], calculateWinProbOnServe: (Int, Int) => Double): Double = {

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
    throw new UnsupportedOperationException("Not implemented yet.")
  }

}