package dk.tennis.compare.old.bulkcompare

import dk.atp.api._
import dk.atp.api.facts.AtpFactsApi._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob._
import java.util.Date
import org.joda.time.DateTime
import dk.atp.api.tournament.TournamentAtpApi._
import ATPTennisMatchCompare._
import java.util.Date
import dk.atp.api.domain._
import dk.tennis.compare.old.bulkcompare.TennisPlayerCompare

/**
 * Calculates probability of winning a tennis match by player A against player B. For instance Roger Federer vs Novak Djokovic
 *
 */
object ATPTennisMatchCompare {
  case class TimestampedDouble(timestamp: Date, value: Double)
}

/**
 * @param histDataInMonths For how many months historical tennis data should be used to calculate tennis probabilities.
 * @param discountFactor How much discount old market data. 1 = no discount.
 * @param discountPeriodInDays With what time interval historical data should be discounted.
 *
 */
class ATPTennisMatchCompare(atpMatchLoader: ATPMatchesLoader, histDataInMonths:Int=12, discountFactor: Double = 1, discountPeriodInDays: Int = 7) extends TennisPlayerCompare {

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

    val matchTimeFrom = new DateTime(marketTime.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(marketTime.getTime()).minusDays(1)

    val matches = getMatches(surface, matchTimeFrom, matchTimeTo)

    val winOnReturnAvgProb = calcWinOnReturnAvgProb(matches)

    val playerAwinOnServeProb = winOnServeProb(fullNamePlayerA, matches)
    val playerBwinOnServeProb = winOnServeProb(fullNamePlayerB, matches)
    val playerAwinOnReturnProb = winOnReturnProb(fullNamePlayerA, matches)
    val playerBwinOnReturnProb = winOnReturnProb(fullNamePlayerB, matches)

    val winOnServeAGivenBProb = TennisProbFormulaCalc.pointProb(playerAwinOnServeProb, playerBwinOnReturnProb, winOnReturnAvgProb)
    val winOnServeBGivenAProb = TennisProbFormulaCalc.pointProb(playerBwinOnServeProb, playerAwinOnReturnProb, winOnReturnAvgProb)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(winOnServeAGivenBProb, 1 - winOnServeBGivenAProb, matchType)
    matchProbAGivenB

  }

  private def winOnServeProb(playerName: String, matches: List[MatchComposite]): Double = {
    val playerMatches = matches.filter(m => m.matchFacts.containsPlayer(playerName))
    val timestampedWinOnServePct = playerMatches.map { m =>
      val playerFacts = m.matchFacts.playerFacts(playerName).get
      TimestampedDouble(m.tournament.tournamentTime, playerFacts.totalServicePointsWonPct)
    }.filter(tsValue => !tsValue.value.isNaN())
    val winOnServeProb = avgDiscount(timestampedWinOnServePct)
    winOnServeProb
  }

  private def winOnReturnProb(playerName: String, matches: List[MatchComposite]): Double = {
    val playerMatches = matches.filter(m => m.matchFacts.containsPlayer(playerName))
    val timestampedWinOnReturnPct = playerMatches.map { m =>
      val opponentFacts = m.matchFacts.playerOpponentFacts(playerName).get
      TimestampedDouble(m.tournament.tournamentTime, opponentFacts.totalServicePointsLostPct)
    }.filter(tsValue => !tsValue.value.isNaN())
    val winOnReturnProb = avgDiscount(timestampedWinOnReturnPct)
    winOnReturnProb
  }

  private def calcWinOnReturnAvgProb(matches: List[MatchComposite]): Double = {
    val timestampedWinOnReturnPct = matches.flatMap { m =>
      TimestampedDouble(m.tournament.tournamentTime, m.matchFacts.playerAFacts.totalServicePointsLostPct) ::
        TimestampedDouble(m.tournament.tournamentTime, m.matchFacts.playerBFacts.totalServicePointsLostPct) :: Nil
    }.filter(tsValue => !tsValue.value.isNaN())

    val winOnReturnProb = avgDiscount(timestampedWinOnReturnPct)
    winOnReturnProb
  }

  private def getMatches(surface: SurfaceEnum, matchTimeFrom: DateTime, matchTimeTo: DateTime): List[MatchComposite] = {
    val matches = (matchTimeFrom.getYear() to matchTimeTo.getYear()).flatMap { year =>
      atpMatchLoader.loadMatches(year).filter(m => m.tournament.surface.equals(surface))
    }

    val filteredByTimeRangeMatches = matches.filter(m => m.tournament.tournamentTime.getTime() > matchTimeFrom.getMillis()
      && m.tournament.tournamentTime.getTime() < matchTimeTo.getMillis())
    filteredByTimeRangeMatches.toList
  }

  def avgDiscount(values: List[TimestampedDouble]): Double = {

    values match {
      case Nil => Double.NaN
      case values => {
        val sortedValues = values.sortWith((a, b) => a.timestamp.getTime > b.timestamp.getTime)
        val maxTime = sortedValues.head.timestamp

        val weightedValues = sortedValues.map { tsValue =>
          val deltaTime = maxTime.getTime() - tsValue.timestamp.getTime()
          val discountLevel = (deltaTime / 1000/ 3600 / 24) / discountPeriodInDays
          val discountValue = Math.pow(discountFactor,discountLevel)
          (discountValue, tsValue)
        }
        val weightsSum = weightedValues.map { case (weight, tsValue) => weight }.sum
        val discountAvg = weightedValues.map { case (weight, tsValue) => weight * tsValue.value }.sum / weightsSum
        discountAvg
      }
    }

  }

}
