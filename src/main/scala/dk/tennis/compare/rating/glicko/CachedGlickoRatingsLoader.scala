package dk.tennis.compare.rating.glicko

import java.util.Date

import scala.collection.Map
import scala.collection.mutable

import org.joda.time.DateTime

import GlickoRating.Rating
import GlickoRating.Result
import dk.atp.api.ATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum.SurfaceEnum

object CachedGlickoRatingsLoader {
  def apply(
    atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12,
    initialGlickoRating: Double = 1500, initialGlickoDeviation: Double = 350, discountConstant: Double, discountDurationInDays: Int = 7): CachedGlickoRatingsLoader = {
    val glickoRating = new GenericGlickoRating(initialGlickoRating, initialGlickoDeviation, discountConstant, discountDurationInDays)
    new CachedGlickoRatingsLoader(glickoRating, atpMatchLoader, histDataInMonths)
  }
}

class CachedGlickoRatingsLoader(glicko: GenericGlickoRating, atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12) extends GlickoRatingsLoader {

  /**key - timestamp + surface.*/
  private val cachedRatings: mutable.Map[String, Map[String, Tuple2[Rating, Rating]]] = mutable.Map()
  
  /**
   * @return  [playerName,[ratingOnServe,ratingOnReturn]]
   */
  def ratings(timestamp: Date, surface: SurfaceEnum): Map[String, Tuple2[Rating, Rating]] = {
    val key = timestamp.getTime() + surface.toString()
    cachedRatings.getOrElseUpdate(key,loadRatings(timestamp, surface))
  }

  private def loadRatings(timestamp: Date, surface: SurfaceEnum): Map[String, Tuple2[Rating, Rating]] = {
   
    val matchTimeFrom = new DateTime(timestamp.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(timestamp.getTime()).minusDays(1)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo).sortWith((a, b) => a.tournament.tournamentTime.getTime() < b.tournament.tournamentTime.getTime())

    val eloResults = matches.flatMap { m =>
      val results =
        Result(m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.totalServicePointsWonPct, m.tournament.tournamentTime) ::
          Result(m.matchFacts.playerBFacts.playerName, m.matchFacts.playerAFacts.playerName, m.matchFacts.playerBFacts.totalServicePointsWonPct, m.tournament.tournamentTime) :: Nil

      results
    }.filter(!_.score.isNaN())

    val ratings = glicko.calcServeReturnRatings(eloResults)
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