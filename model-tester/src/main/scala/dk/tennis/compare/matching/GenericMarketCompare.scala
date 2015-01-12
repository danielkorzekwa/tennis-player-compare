package dk.tennis.compare.matching

import scala.io.Source
import org.apache.commons.io.FileUtils._
import java.io.File
import scala.collection.JavaConversions._
import dk.atp.api.facts.AtpFactsApi._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import java.util.Date
import org.apache.commons.math.util._
import dk.atp.api.tournament.TournamentAtpApi._
import scala.collection.immutable.TreeMap
import org.joda.time.DateTime
import scala.math._
import dk.atp.api.ATPMatchesLoader
import dk.tennis.compare.domain.BfMarket
import dk.atp.api.domain.TennisMatch

object GenericMarketCompare extends MarketCompare {

  def compare(atpMarket: TennisMatch, betfairMarket: BfMarket): Double = {

    def playerNames(matchFacts: TennisMatch): List[String] = matchFacts.player1 :: matchFacts.player2 :: Nil

    val playerNamesMatched = playerNames(atpMarket).sorted.equals(betfairMarket.runnerMap.values.toList.map(r => r.name).sorted)

    if (playerNamesMatched) {
      val timeDiff = abs(betfairMarket.scheduledOff.getTime() - atpMarket.tournament.tournamentTime.getTime())

      /**Time difference between matched markets must be lower than 30 days.*/
      1d / ((timeDiff / 1000 / 3600 / 24) + 1.01)
    } else 0
  }
}