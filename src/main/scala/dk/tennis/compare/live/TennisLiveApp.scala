package dk.tennis.compare.live

import scala.collection.JavaConversions._
import org.slf4j._
import dk.bot.betfairservice.BetFairService
import dk.bot.betfairservice.model._
import java.util.Date
import java.util.Arrays
import java.util.HashSet
import dk.bot.betfairservice.model.BFMUBet
import dk.bot.betfairservice.model.BFBetStatus
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.io.Source
import dk.atp.api.facts._
import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.atp.api.domain._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare._
import java.util.Date
import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.atp.api._

/**Running this app from maven: mvn -Dexec.mainClass=dk.tennis.compare.live.TennisLiveApp exec:java*/
object TennisLiveApp extends App {

  new TennisLiveTrader().run()
  
 
}