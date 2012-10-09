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

  private val priorProb = List(0.6522, 0.2059, 0.1076, 0.0343)
  private val emissionProb = List(0.5037, 0.4963, 0.3639, 0.6361, 0.2110, 0.7890, 0.0629, 0.9371, 0.6388, 0.3612, 0.4867, 0.5133, 0.3905, 0.6095, 0.1125, 0.8875, 0.8906, 0.1094, 0.5336, 0.4664, 0.5291, 0.4709, 0.2255, 0.7745, 0.9468, 0.0532, 0.8106, 0.1894, 0.7339, 0.2661, 0.5591, 0.4409)
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