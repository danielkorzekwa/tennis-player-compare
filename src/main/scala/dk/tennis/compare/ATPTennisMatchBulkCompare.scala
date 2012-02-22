package dk.tennis.compare

import scala.io.Source
import org.apache.commons.io.FileUtils._
import java.io.File
import scala.collection.JavaConversions._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import java.util.Date
import domain._
import dk.atp.api.AtpWorldTourApiImpl
import org.apache.commons.math.util._
import dk.atp.api.tournament.TournamentAtpApi._
import scala.collection.immutable.TreeMap
import org.joda.time.DateTime
import scala.Math._
import dk.atp.api.ATPMatchesLoader

/**
 * Calculates tennis market probabilities for a list of markets in a batch process.
 *
 * @author KorzekwaD
 */
class ATPTennisMatchBulkCompare(tennisMatchCompare: TennisPlayerCompare, atpMatchLoader: ATPMatchesLoader) extends TennisMatchBulkCompare {

  /**
   * Calculates tennis market probabilities for a list of markets in a batch process and exports it to CSV file.
   *
   *  @param marketDataFileIn File path to CSV file with market records. Input market data CSV columns:
   *  market_id, market_name, market_time (yyyy-mm-dd hh:mm:ss, e.g. 2011-12-21 18:19:09),selection_id, selection_name
   *  There must be two records for every market, one record for each selection.
   *
   *  @param marketProbFileOut CVS file for exporting market probabilities.
   *  Columns: The same columns as for input file, and:  'probability' of winning a tennis match, surface (CLAY,GRASS,HARD), matchType (THREE_SET_MATCH/FIVE_SET_MATCH).
   *
   *  @param progress Number of markets remaining for processing..
   *
   */
  def matchProb(marketDataFileIn: String, marketProbFileOut: String, progress: (Int) => Unit): Unit = {
    val marketDataSource = Source.fromFile(marketDataFileIn)

    val markets = Market.fromCSV(marketDataSource.getLines().drop(1).toList)
    val marketsSize = markets.size
    val marketProbabilities = for ((market, index) <- markets.zipWithIndex) yield {
      progress(marketsSize - index)
      val tournament =lookup(market)

      toMarketProb(market, tournament)
    }

    val filteredAndSortedMarketProbs = marketProbabilities.filter(p => p.isDefined && p.get.runnerProbs.values.exists(!_.isNaN)).
    sortWith(_.get.market.scheduledOff.getTime() < _.get.market.scheduledOff.getTime)
    val marketProbsData = filteredAndSortedMarketProbs.flatMap(p => p.get.toCSV())

    val marketProbFile = new File(marketProbFileOut)
    val header = "event_id,full_description,scheduled_off,selection_id,selection,probability, surface, match_type"
    writeLines(marketProbFile, header :: marketProbsData)
  }

  private def toMarketProb(m: Market, tournament: Option[Tournament]): Option[MarketProb] = {
    val marketProb: Option[MarketProb] = try {
      val runners = m.runnerMap.keys.toList

      val matchType = tournament.get.numOfSet match {
        case 2 => THREE_SET_MATCH
        case 3 => FIVE_SET_MATCH
      }
      val probability = tennisMatchCompare.matchProb(m.runnerMap(runners(0)), m.runnerMap(runners(1)), tournament.get.surface, matchType, m.scheduledOff)

      Option(MarketProb(m, Map(runners(0) -> probability, runners(1) -> (1 - probability)), tournament.get.surface, matchType))
    } catch { case _ => None }
    marketProb
  }
  
   /**Look for tournament matching given market.*/
  private def lookup(market: Market): Option[Tournament] = {
    val year = new DateTime(market.scheduledOff).getYear()
  
    val matches = atpMatchLoader.loadMatches(year)

    def playerNames(matchFacts: MatchFacts): List[String] = matchFacts.playerAFacts.playerName :: matchFacts.playerBFacts.playerName :: Nil

    val filteredMatches = matches.filter(m => playerNames(m.matchFacts).sorted.equals(market.runnerMap.values.toList.sorted))
    val timeDiffMatches = TreeMap[Long, Tournament]() ++ filteredMatches.map(m => (abs(market.scheduledOff.getTime() - m.tournament.tournamentTime.getTime()), m.tournament))

    if (timeDiffMatches.isEmpty) None else Option(timeDiffMatches.head._2)
  } 

}