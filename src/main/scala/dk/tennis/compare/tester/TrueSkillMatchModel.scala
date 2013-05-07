package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import scala.math._
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.rating.TrueSkill
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennis.compare.pointprob.GenericPointProbCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

case class TrueSkillMatchModel extends MatchModel {

  private val skillTransVariance = pow(25d / 300, 2)
  private val performanceVariance = pow(25d / 16, 2)
  private val trueSkillModel = GenericTrueSkill(skillTransVariance, performanceVariance)

  def matchProb(m: MatchComposite): Option[Double] = {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(playerAFacts.playerName)
    val playerBSkill = ratingsMap.get(playerBFacts.playerName)

    val prob = if (playerASkill.isDefined && playerBSkill.isDefined) {

      val winProb = GenericTrueSkillMatchProb(skillTransVariance, performanceVariance).matchProb(playerASkill.get, playerBSkill.get)

      /**Enhance prob for 5 sets match.*/
      if (m.tournament.numOfSet == 3) {
        val pointProb = GenericPointProbCalc.calcPointProb(winProb, THREE_SET_MATCH)
        val matchProb = TennisProbFormulaCalc.matchProb(pointProb, pointProb, FIVE_SET_MATCH)
        Some(matchProb)
      } else Some(winProb)

    } else None

    prob
  }

  def addMatchResult(m: MatchComposite) {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    val playerAWinner: Boolean = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    trueSkillModel.addResult(Result(playerAFacts.playerName, playerBFacts.playerName, playerAWinner))
  }

  def getTrueSkillModel(): TrueSkill = trueSkillModel
}