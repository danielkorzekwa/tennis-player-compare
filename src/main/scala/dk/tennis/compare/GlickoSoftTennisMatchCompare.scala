package dk.tennis.compare
import java.util.Date
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.atp.api.ATPMatchesLoader
import org.joda.time._
import dk.atp.api.domain.MatchComposite
import glicko.GlickoRating._
import dk.tennis.compare.glicko.GenericGlickoRating
import dk.tennisprob._

/**
 * @param histDataInMonths For how many months historical tennis data should be used to calculate tennis probabilities.
 */
class GlickoSoftTennisMatchCompare(atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12, initialGlickoRating: Double = 1500, initialGlickoDeviation: Double = 350) extends TennisPlayerCompare {

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
    val matchTimeFrom = new DateTime(marketTime.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(marketTime.getTime()).minusDays(1)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo).sortWith((a, b) => a.tournament.tournamentTime.getTime() < b.tournament.tournamentTime.getTime())

    val eloResults = matches.flatMap { m =>
      val results =
        Result(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.totalServicePointsWonPct) ::
          Result(m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.totalServicePointsWonPct) :: Nil

      results
    }.filter(!_.score.isNaN())

    val glicko = new GenericGlickoRating(initialGlickoRating, initialGlickoDeviation)
    val ratings = glicko.calcServeReturnRatings(eloResults)

    val playerARating = ratings(fullNamePlayerA)
    val playerBRating = ratings(fullNamePlayerB)
    val playerAOnServeProb = glicko.expectedScore(playerARating._1.rating, playerBRating._2.rating, playerBRating._2.deviation)
    val playerBOnServeProb = glicko.expectedScore(playerBRating._1.rating, playerARating._2.rating, playerARating._2.deviation)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(playerAOnServeProb, 1 - playerBOnServeProb, matchType)
    matchProbAGivenB
  }

  private def getMatches(surface: SurfaceEnum, matchTimeFrom: DateTime, matchTimeTo: DateTime): List[MatchComposite] = {
    val matches = (matchTimeFrom.getYear() to matchTimeTo.getYear()).flatMap { year =>
      atpMatchLoader.loadMatches(year).filter(m => m.tournament.surface.equals(surface))
    }

    val filteredByTimeRangeMatches = matches.filter(m => m.tournament.tournamentTime.getTime() > matchTimeFrom.getMillis()
      && m.tournament.tournamentTime.getTime() < matchTimeTo.getMillis())
    filteredByTimeRangeMatches.toList
  }
}