package dk.tennis.compare.old.bulkcompare
import scala.collection.mutable.Map
import dk.atp.api.ATPMatchesLoader
import dk.atp.api.domain._

class CachedATPMatchesLoader(atpMatchesLoader: ATPMatchesLoader) extends ATPMatchesLoader {

  /**key - year.*/
  private val cachedMatches: Map[Int, List[MatchComposite]] = Map()

  def loadMatches(year: Int): List[MatchComposite] = cachedMatches.getOrElseUpdate(year, atpMatchesLoader.loadMatches(year))
}