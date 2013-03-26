package dk.tennis.compare.rating.glicko2

import java.util.Date

import scala.collection.Map
import scala.collection.mutable

import org.joda.time.DateTime

import Glicko2Rating.PlayerRating
import Glicko2Rating.Result
import dk.atp.api.ATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum

object CachedGlicko2RatingsLoader {
  def apply(
    atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12,
    initialRating: Double = 0, initialDeviation: Double = 350d / 173.7178, initialVolatility: Double = 0.06,tau:Double=0.5,discountDurationInDays: Int): CachedGlicko2RatingsLoader = {
    val glickoRating = new GenericGlicko2Rating(initialRating, initialDeviation, initialVolatility,tau,discountDurationInDays)
    new CachedGlicko2RatingsLoader(glickoRating, atpMatchLoader, histDataInMonths)
  }
}

class CachedGlicko2RatingsLoader(glicko2: GenericGlicko2Rating, atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12) extends Glicko2RatingsLoader {

  /**key - timestamp + surface.*/
  private val cachedRatings: mutable.Map[String, Map[String, PlayerRating]] = mutable.Map()
  
  /**
   * @return  [playerName,playerRating]
   */
  def ratings(timestamp: Date, surface: SurfaceEnum): Map[String, PlayerRating] = {
    val key = timestamp.getTime() + surface.toString()
    cachedRatings.getOrElseUpdate(key,loadRatings(timestamp, surface))
  }

  private def loadRatings(timestamp: Date, surface: SurfaceEnum): Map[String, PlayerRating] = {
   
    val matchTimeFrom = new DateTime(timestamp.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(timestamp.getTime()).minusDays(1)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo).sortWith((a, b) => a.tournament.tournamentTime.getTime() < b.tournament.tournamentTime.getTime())

    val glickoResults = matches.flatMap { m =>
      val results =
        Result(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.totalServicePointsWonPct, m.tournament.tournamentTime) ::
          Result(m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.totalServicePointsWonPct, m.tournament.tournamentTime) :: Nil

      results
    }.filter(!_.score.isNaN())

    val ratings = glicko2.calcServeReturnRatings(glickoResults)
    ratings
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