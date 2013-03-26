package dk.tennis.compare
import java.util.Date
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.atp.api.ATPMatchesLoader
import org.joda.time._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.elo.EloRating._
import dk.tennis.compare.rating.elo.GenericEloRating

/**
 * @param histDataInMonths For how many months historical tennis data should be used to calculate tennis probabilities.
 */
class EloTennisMatchCompare(atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12,eloKFactor:Double=48) extends TennisPlayerCompare {

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

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo)
    val eloResults = matches.map { m =>
      val won = if (m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)) 1 else 0
      Result(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName, won)
    }
    val ratings = new GenericEloRating(eloKFactor).calcRatings(eloResults)

    val playerARating = ratings(fullNamePlayerA)
    val playerBRating = ratings(fullNamePlayerB)
    val matchProb = new GenericEloRating().calcExpectedScore(playerARating, playerBRating)
    matchProb
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