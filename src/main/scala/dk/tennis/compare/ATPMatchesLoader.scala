package dk.tennis.compare
import dk.atp.api.TournamentAtpApi._
import ATPMatchesLoader._
/**Load matches from http://www.atpworldtour.com/ web site.*/

object ATPMatchesLoader {
   case class MatchComposite(tournament: Tournament, tennisMatch: Match, matchFacts: MatchFacts)
}

trait ATPMatchesLoader {

   def loadMarkets(year: Int): List[MatchComposite]
  
}