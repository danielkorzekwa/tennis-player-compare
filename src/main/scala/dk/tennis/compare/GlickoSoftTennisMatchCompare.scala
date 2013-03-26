package dk.tennis.compare

import java.util.Date

import dk.atp.api.domain.SurfaceEnum.SurfaceEnum
import dk.tennis.compare.rating.glicko.GenericGlickoRating
import dk.tennisprob.TennisProbCalc.MatchTypeEnum.MatchTypeEnum
import dk.tennisprob.TennisProbFormulaCalc
import rating.glicko.GlickoRatingsLoader

class GlickoSoftTennisMatchCompare(glickoRatingLoader: GlickoRatingsLoader) extends TennisPlayerCompare {

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

    val ratings = glickoRatingLoader.ratings(marketTime, surface)

    val playerARating = ratings(fullNamePlayerA)
    val playerBRating = ratings(fullNamePlayerB)
    val playerAOnServeProb = GenericGlickoRating.expectedScore(playerARating._1.rating, playerBRating._2.rating, playerBRating._2.deviation)
    val playerBOnServeProb = GenericGlickoRating.expectedScore(playerBRating._1.rating, playerARating._2.rating, playerARating._2.deviation)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, matchType)
    matchProbAGivenB
  }

}