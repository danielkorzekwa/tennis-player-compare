package dk.tennis.compare.old.bulkcompare

import java.util.Date
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum
import dk.tennisprob.TennisProbCalc.MatchTypeEnum.MatchTypeEnum
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennis.compare.rating.markov.MarkovRatingsLoader

class MarkovTennisMatchCompare(markovRatingLoader: MarkovRatingsLoader) extends TennisPlayerCompare {

  /**
   * Calculates probability of winning a tennis match by player A against player B.
   *
   * @param fullNamePlayerA e.g. Roger Federer
   * @param fullNamePlayerB e.g. Novak Djokovic
   * @param surface Clay, grass or hard.
   * @param matchType Three or five set match.
   * @param marketTime When the tennis match was played.
   *
   * @return Probability between 0 and 1.
   */
  def matchProb(fullNamePlayerA: String, fullNamePlayerB: String, surface: SurfaceEnum, matchType: MatchTypeEnum, marketTime: Date): Double = {

    val ratings = markovRatingLoader.ratings(marketTime, surface)

    val playerARating = ratings(fullNamePlayerA)
    val playerBRating = ratings(fullNamePlayerB)
    val playerAOnServeProb = markovRatingLoader.getMarkovRating().calcWinProb(playerARating.ratingOnServe, playerBRating.ratingOnReturn)
    val playerBOnServeProb = markovRatingLoader.getMarkovRating().calcWinProb(playerBRating.ratingOnServe, playerARating.ratingOnReturn)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, matchType)
    matchProbAGivenB
  }

}