package dk.tennis.compare.old.bulkcompare

import java.util.Date
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum
import dk.tennisprob.TennisProbCalc.MatchTypeEnum.MatchTypeEnum
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennis.compare.rating.glicko2.Glicko2RatingsLoader
import dk.tennis.compare.rating.glicko2.GenericGlicko2Rating

class Glicko2SoftTennisMatchCompare(glicko2RatingLoader: Glicko2RatingsLoader) extends TennisPlayerCompare {

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

    val ratings = glicko2RatingLoader.ratings(marketTime, surface)

    val playerARating = ratings(fullNamePlayerA)
    val playerBRating = ratings(fullNamePlayerB)
    val playerAOnServeProb = GenericGlicko2Rating.E(playerARating.ratingOnServe.rating, playerBRating.ratingOnReturn.rating, playerBRating.ratingOnReturn.deviation)
    val playerBOnServeProb = GenericGlicko2Rating.E(playerBRating.ratingOnServe.rating, playerARating.ratingOnReturn.rating, playerARating.ratingOnReturn.deviation)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, matchType)
    matchProbAGivenB
  }

}