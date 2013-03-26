package dk.tennis.compare.rating.markov

import java.util.Date
import MarkovRating._
import dk.atp.api.domain.SurfaceEnum
import dk.atp.api.ATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import org.joda.time.DateTime
import dk.atp.api.domain.SurfaceEnum._
import scala.collection._

/**
 * Returns tennis ratings at a given time. Ratings are cached by rating request time.
 *
 */
class CachedMarkovRatingsLoader(markovRating: GenericMarkovRating, atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12) extends MarkovRatingsLoader {

  /**key - timestamp + surface.*/
  private val cachedRatings: mutable.Map[String, Map[String, PlayerRating]] = mutable.Map()

  /**
   * Returns tennis ratings at a given time. Ratings are cached by rating request time.
   * @see MarkovRatingsLoader.ratings()
   */
  def ratings(timestamp: Date, surface: SurfaceEnum): Map[String, PlayerRating] = {
    val key = timestamp.getTime() + surface.toString()
    cachedRatings.getOrElseUpdate(key, loadRatings(timestamp, surface))
  }

  private def loadRatings(timestamp: Date, surface: SurfaceEnum): Map[String, PlayerRating] = {

    val matchTimeFrom = new DateTime(timestamp.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(timestamp.getTime()).minusDays(1)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo).sortWith((a, b) => a.tournament.tournamentTime.getTime() < b.tournament.tournamentTime.getTime())

    val markovResults = matches.flatMap { m =>

      val playerAFacts = m.matchFacts.playerAFacts
      val playerBFacts = m.matchFacts.playerBFacts

      val results =
        Result(playerAFacts.playerName, playerBFacts.playerName, playerAFacts.totalServicePointsWon, 
            playerAFacts.totalServicePoints - playerAFacts.totalServicePointsWon, m.tournament.tournamentTime) ::
          Result(playerBFacts.playerName,playerAFacts.playerName, playerBFacts.totalServicePointsWon, 
              playerBFacts.totalServicePoints - playerBFacts.totalServicePointsWon, m.tournament.tournamentTime) :: Nil

      results
    }

    val ratings = markovRating.calcPlayerRatings(markovResults)
    ratings

  }

  def getMarkovRating(): MarkovRating = markovRating

  private def getMatches(surface: SurfaceEnum, matchTimeFrom: DateTime, matchTimeTo: DateTime): List[MatchComposite] = {
    val matches = (matchTimeFrom.getYear() to matchTimeTo.getYear()).flatMap { year =>
      atpMatchLoader.loadMatches(year).filter(m => m.tournament.surface.equals(surface))
    }

    val filteredByTimeRangeMatches = matches.filter(m => m.tournament.tournamentTime.getTime() > matchTimeFrom.getMillis()
      && m.tournament.tournamentTime.getTime() < matchTimeTo.getMillis())
    filteredByTimeRangeMatches.toList
  }
}