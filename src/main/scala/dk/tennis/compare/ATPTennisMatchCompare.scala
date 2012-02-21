package dk.tennis.compare

import dk.atp.api._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob._
import java.util.Date
import org.joda.time.DateTime
import dk.atp.api.tournament.TournamentAtpApi._
import ATPMatchesLoader._
import ATPTennisMatchCompare._
import java.util.Date

/**
 * Calculates probability of winning a tennis match by player A against player B. For instance Roger Federer vs Novak Djokovic
 *
 */
object ATPTennisMatchCompare {
  case class TimestampedDouble(timestamp: Date, value: Double)
}

/**
 * @param discountFactor How much discount old market data. 1 = no discount.
 */
class ATPTennisMatchCompare(atpMatchLoader: ATPMatchesLoader, discountFactor: Double = 1) extends TennisPlayerCompare {

  /**
   * Calculates probability of winning a tennis match by player A against player B.
   *
   * @param fullNamePlayerA e.g. Roger Federer
   * @param fullNamePlayerB e.g. Novak Djokovic
   * @param surface Clay, grass or hard.
   * @param matchType Three or five set match.
   * @param year Probability is calculated for the last day of a year.
   * @return Probability between 0 and 1.
   */
  def matchProb(fullNamePlayerA: String, fullNamePlayerB: String, surface: SurfaceEnum, matchType: MatchTypeEnum, marketTime: Date): Double = {

    val matchTimeFrom = new DateTime(marketTime.getTime()).minusMonths(12)
    val matchTimeTo = new DateTime(marketTime.getTime()).minusDays(7)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo)

    val winOnReturnAvgProb = calcWinOnReturnAvgProb(matches)

    val winOnServeAGivenBProb = TennisProbFormulaCalc.pointProb(winOnServeProb(fullNamePlayerA, matches), winOnReturnProb(fullNamePlayerB, matches), winOnReturnAvgProb)
    val winOnServeBGivenAProb = TennisProbFormulaCalc.pointProb(winOnServeProb(fullNamePlayerB, matches), winOnReturnProb(fullNamePlayerA, matches), winOnReturnAvgProb)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(winOnServeAGivenBProb, 1 - winOnServeBGivenAProb, matchType)
    matchProbAGivenB

  }

  private def winOnServeProb(playerName: String, matches: List[MatchComposite]): Double = {
    val playerMatches = matches.filter(m => m.matchFacts.containsPlayer(playerName))
    val timestampedWinOnServePct = playerMatches.map { m =>
      val playerFacts = m.matchFacts.playerFacts(playerName).get
      TimestampedDouble(m.tournament.tournamentTime, playerFacts.totalServicePointsWon.toDouble / playerFacts.totalServicePoints.toDouble)
    }
    val winOnServeProb = avg(timestampedWinOnServePct)
    winOnServeProb
  }

  private def winOnReturnProb(playerName: String, matches: List[MatchComposite]): Double = {
    val playerMatches = matches.filter(m => m.matchFacts.containsPlayer(playerName))   
    val timestampedWinOnReturnPct = playerMatches.map { m =>
      val opponentFacts = m.matchFacts.playerOpponentFacts(playerName).get
      TimestampedDouble(m.tournament.tournamentTime, (opponentFacts.totalServicePoints - opponentFacts.totalServicePointsWon).toDouble / opponentFacts.totalServicePoints.toDouble)
    }
    val winOnServeProb = avg(timestampedWinOnReturnPct)
    winOnServeProb
  }

  private def calcWinOnReturnAvgProb(matches: List[MatchComposite]): Double = {
    val playerFacts = matches.flatMap(m => List(m.matchFacts.playerAFacts, m.matchFacts.playerBFacts))
    val prob = playerFacts.map(f => (f.totalServicePoints - f.totalServicePointsWon).toDouble / f.totalServicePoints).sum / playerFacts.size
    prob
  }

  private def getMatches(surface: SurfaceEnum, matchTimeFrom: DateTime, matchTimeTo: DateTime): List[MatchComposite] = {
    val matches = (matchTimeFrom.getYear() to matchTimeTo.getYear()).flatMap { year =>
      atpMatchLoader.loadMarkets(year).filter(m => m.tournament.surface.equals(surface))
    }

    val filteredByTimeRangeMatches = matches.filter(m => m.tournament.tournamentTime.getTime() > matchTimeFrom.getMillis()
      && m.tournament.tournamentTime.getTime() < matchTimeTo.getMillis())
    filteredByTimeRangeMatches.toList
  }

  def avg(values: List[TimestampedDouble]): Double = {
    val sortedValues = values.sortWith((a, b) => a.timestamp.getTime > b.timestamp.getTime)
    values.map(_.value).sum / values.size
  }

}