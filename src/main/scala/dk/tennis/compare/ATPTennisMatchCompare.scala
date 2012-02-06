package dk.tennis.compare

import dk.atp.api._
import dk.atp.api.AtpWorldTourApi._
import SurfaceEnum._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennisprob._
import java.util.Date
import org.joda.time.DateTime
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
  def matchProb(fullNamePlayerA: String, fullNamePlayerB: String, surface: SurfaceEnum, matchType: MatchTypeEnum, marketTime:Date): Double = {

    val year = new DateTime().withMillis(marketTime.getTime()).getYear()
    
    /**@return Tuple2[winOnServeAGivenBProb,winOnServeBGivenAProb]*/
    def winOnServe(): Tuple2[Double, Double] = {

      val playerAFacts = atpApi.playerFacts(fullNamePlayerA, surface, year)
      val playerBFacts = atpApi.playerFacts(fullNamePlayerB, surface, year)

      require(playerAFacts.serviceGamesPlayed > 0 && playerAFacts.returnGamesPlayed > 0, "Number of games served/returned by player %s is zero.".format(fullNamePlayerA))
      require(playerBFacts.serviceGamesPlayed > 0 && playerBFacts.returnGamesPlayed > 0, "Number of games served/returned by player %s is zero.".format(fullNamePlayerB))

      val firstServeAvpProb = atpApi.firstServeFacts(surface, year).firstServeAvgPct() / 100

      val firstReturnAvgProb = atpApi.pointWonFacts(PointWonFactEnum.FIRST_SERVE_RETURN_POINTS_WON, surface, year).pointWonAvgPct() / 100
      val secondReturnAvgProb = atpApi.pointWonFacts(PointWonFactEnum.SECOND_SERVE_RETURN_POINTS_WON, surface, year).pointWonAvgPct() / 100
      val winOnReturnAvgProb = TennisProbFormulaCalc.pointAvgProb(firstServeAvpProb, firstReturnAvgProb, secondReturnAvgProb)

      //calculate match prob
      val winOnServeAGivenBProb = TennisProbFormulaCalc.pointProb(winOnServeProb(playerAFacts), winOnReturnProb(playerBFacts,firstServeAvpProb), winOnReturnAvgProb)
      val winOnServeBGivenAProb = TennisProbFormulaCalc.pointProb(winOnServeProb(playerBFacts), winOnReturnProb(playerAFacts,firstServeAvpProb), winOnReturnAvgProb)

      Tuple2(winOnServeAGivenBProb, winOnServeBGivenAProb)
    }

    val (winOnServeAGivenBProb, winOnServeBGivenAProb) = winOnServe()
    val matchProbAGivenB = TennisProbFormulaCalc.matchProb(winOnServeAGivenBProb, 1 - winOnServeBGivenAProb, matchType)
    matchProbAGivenB
  }

  def winOnServeProb(playerFacts: PlayerFacts): Double = TennisProbFormulaCalc.pointAvgProb(playerFacts.firstServePct / 100, playerFacts.firstServeWonPct / 100, playerFacts.secondServeWonPct / 100)
  def winOnReturnProb(playerFacts: PlayerFacts, firstServeAvpProb: Double) = TennisProbFormulaCalc.pointAvgProb(firstServeAvpProb, playerFacts.firstReturnWonPct / 100, playerFacts.secondReturnWonPct / 100)
}