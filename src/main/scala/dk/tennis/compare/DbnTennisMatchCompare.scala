package dk.tennis.compare

import dk.atp.api.facts.AtpFactsApi._
import dk.atp.api.domain.SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import java.util.Date
import dk.tennis.em.dbn.infer.grmm.GrmmInferDbnTennisFactory
import dk.atp.api.ATPMatchesLoader
import org.joda.time.DateTime
import dk.atp.api.domain.MatchComposite
import dk.tennis.em.dbn.factorgraph.DbnTennis._
import org.joda.time.Duration

class DbnTennisMatchCompare(atpMatchLoader: ATPMatchesLoader, histDataInMonths: Int = 12) extends TennisPlayerCompare {

  private val priorProb = List(0.4939, 0.4751, 0.0310)
  private val emissionProb = List(0.5772, 0.4228, 0.2799, 0.7201, 0.1077, 0.8923, 0.7260, 0.2740, 0.4666, 0.5334, 0.1876, 0.8124, 0.9916, 0.0084, 0.8272, 0.1728, 0.4742, 0.5258)
  private val transitionProb = List(0.9929, 0.0069, 0.0002, 0.0105, 0.9801, 0.0094, 0.0002, 0.0014, 0.9984)

  def matchProb(fullNamePlayerA: String, fullNamePlayerB: String, surface: SurfaceEnum, matchType: MatchTypeEnum, marketTime: Date): Double = {

    val matchTimeFrom = new DateTime(marketTime.getTime()).minusMonths(histDataInMonths)
    val matchTimeTo = new DateTime(marketTime.getTime()).minusDays(1)
    val matches = getMatches(surface, matchTimeFrom, matchTimeTo).sortWith((a, b) => a.tournament.tournamentTime.getTime() < b.tournament.tournamentTime.getTime())

    val firstMatchTime = matches.head.tournament.tournamentTime.getTime()
    val results = matches.map(m => toResult(firstMatchTime, m)).toList

    val queryResult = Result(fullNamePlayerA, fullNamePlayerB, None, timeslice(firstMatchTime, marketTime.getTime()))
    val inferenceResults = results :+ queryResult
    println("Inference results:" + inferenceResults.size)
    val inferDbnTennis = new GrmmInferDbnTennisFactory().create(inferenceResults, priorProb, emissionProb, transitionProb)
    println("Inference is finished:" + inferenceResults.size)
    val matchProbAGivenB = inferDbnTennis.getPlayerAWinningProb(queryResult.playerA, queryResult.playerB, queryResult.timeSlice)

    matchProbAGivenB
  }

  private def getMatches(surface: SurfaceEnum, matchTimeFrom: DateTime, matchTimeTo: DateTime): List[MatchComposite] = {
    val matches = (matchTimeFrom.getYear() to matchTimeTo.getYear()).flatMap { year =>
      atpMatchLoader.loadMatches(year).filter(m => m.tournament.surface.equals(surface))
    }

    val filteredByTimeRangeMatches = matches.filter(m => m.tournament.tournamentTime.getTime() > matchTimeFrom.getMillis()
      && m.tournament.tournamentTime.getTime() < matchTimeTo.getMillis())
    filteredByTimeRangeMatches.toList
  }

  private def toResult(firstMatchTime: Long, m: MatchComposite): Result = {

    val timeSlice = timeslice(firstMatchTime, new DateTime(m.tournament.tournamentTime).getMillis())

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }

  private def timeslice(firstMatchTime: Long, marketTime: Long): Int = (new Duration(marketTime - firstMatchTime).getStandardDays() / 3000).toInt

  private implicit def bool2int(b: Boolean): Byte = if (b) 1 else 0
}