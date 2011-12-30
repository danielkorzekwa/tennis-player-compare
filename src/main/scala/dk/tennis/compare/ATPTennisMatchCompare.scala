package dk.tennis.compare

import dk.atp.api._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob._

/**
 * Calculates probability of winning a tennis match by player A against player B. For instance Roger Federer vs Novak Djokovic
 *
 */
class ATPTennisMatchCompare(atpApi: AtpWorldTourApiImpl) extends TennisPlayerCompare {

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
  def matchProb(fullNamePlayerA: String, fullNamePlayerB: String, surface: SurfaceEnum, matchType: MatchTypeEnum, year: Int): Double = {

    val playerAFacts = atpApi.playerFacts(fullNamePlayerA, surface, year)
    val playerBFacts = atpApi.playerFacts(fullNamePlayerB, surface, year)

    require(playerAFacts.serviceGamesPlayed > 0 && playerAFacts.returnGamesPlayed > 0, "Number of games served/returned by player % is zero.".format(fullNamePlayerA))
    require(playerBFacts.serviceGamesPlayed > 0 && playerBFacts.returnGamesPlayed > 0, "Number of games served/returned by player % is zero.".format(fullNamePlayerB))

    val firstServeAvpProb = atpApi.firstServeFacts(surface, year).firstServeAvgPct() / 100

    val firstReturnAvgProb = atpApi.pointWonFacts(PointWonFactEnum.FIRST_SERVE_RETURN_POINTS_WON, surface, year).pointWonAvgPct() / 100
    val secondReturnAvgProb = atpApi.pointWonFacts(PointWonFactEnum.SECOND_SERVE_RETURN_POINTS_WON, surface, year).pointWonAvgPct() / 100

    val winOnReturnAvgProb = TennisProbFormulaCalc.pointAvgProb(firstServeAvpProb, firstReturnAvgProb, secondReturnAvgProb)

    val winOnServeProbA = TennisProbFormulaCalc.pointAvgProb(playerAFacts.firstServePct / 100, playerAFacts.firstServeWonPct / 100, playerAFacts.secondServeWonPct / 100)
    val winOnReturnProbA = TennisProbFormulaCalc.pointAvgProb(firstServeAvpProb, playerAFacts.firstReturnWonPct / 100, playerAFacts.secondReturnWonPct / 100)

    val winOnServeProbB = TennisProbFormulaCalc.pointAvgProb(playerBFacts.firstServePct / 100, playerBFacts.firstServeWonPct / 100, playerBFacts.secondServeWonPct / 100)
    val winOnReturnProbB = TennisProbFormulaCalc.pointAvgProb(firstServeAvpProb, playerBFacts.firstReturnWonPct / 100, playerBFacts.secondReturnWonPct / 100)

    //calculate match prob
    val winOnServeAGivenBProb = TennisProbFormulaCalc.pointProb(winOnServeProbA, winOnReturnProbB, winOnReturnAvgProb)
    val winOnServeBGivenAProb = TennisProbFormulaCalc.pointProb(winOnServeProbB, winOnReturnProbA, winOnReturnAvgProb)

    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(winOnServeAGivenBProb, 1 - winOnServeBGivenAProb, matchType)
    matchProbAGivenB
  }
}