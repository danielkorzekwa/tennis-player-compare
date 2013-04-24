package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.rating.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import scala.math._
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb

case class TrueSkillDBNMatchModel extends MatchModel {

  private val performanceVariance = pow(25d / 16, 2)
  //private val trueSkillModel = GenericTrueSkill()

  def matchProb(m: MatchComposite): Option[Double] = {
//
//    val playerAFacts = m.matchFacts.playerAFacts
//    val playerBFacts = m.matchFacts.playerBFacts
//
//    /**key - playerName*/
//    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()
//
//    val playerASkill = ratingsMap.get(playerAFacts.playerName)
//    val playerBSkill = ratingsMap.get(playerBFacts.playerName)
//
//    if (playerASkill.isDefined && playerBSkill.isDefined) {
//
//       val winProb = GenericTrueSkillMatchProb(performanceVariance).matchProb(playerASkill.get, playerBSkill.get)
//
//      Some(winProb)
//    } else None
    
    None
  }

  def addMatchResult(m: MatchComposite) {
//
//    val playerAFacts = m.matchFacts.playerAFacts
//    val playerBFacts = m.matchFacts.playerBFacts
//
//    val playerAWinner: Boolean = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)
//
//    trueSkillModel.addResult(playerAFacts.playerName, playerBFacts.playerName, playerAWinner)
  }
}