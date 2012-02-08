package dk.tennis.compare

import domain._
import dk.atp.api.TournamentAtpApi._

trait TournamentLookup {

  /**Look for tournament matching given market.*/
  def lookup(market:Market):Option[Tournament]
  
}