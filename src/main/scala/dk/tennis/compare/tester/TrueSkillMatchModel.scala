package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.rating.trueskill.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.GenericTrueSkill
import dk.tennis.compare.rating.trueskill.TrueSkill.TrueSkillRating

case class TrueSkillMatchModel extends MatchModel {

  private val trueSkillModel = GenericTrueSkill()

  def matchProb(m: MatchComposite): Option[Double] = {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    /**key - playerName*/
    val ratingsMap: Map[String, TrueSkillRating] = trueSkillModel.getRatings()

    val playerASkill = ratingsMap.get(playerAFacts.playerName)
    val playerBSkill = ratingsMap.get(playerBFacts.playerName)

    if (playerASkill.isDefined && playerBSkill.isDefined) {

      val winProb = trueSkillModel.winnerProb(playerASkill.get, playerBSkill.get)
      Some(winProb)
    } else None
  }

  def addMatchResult(m: MatchComposite) {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    val playerAWinner: Boolean = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    trueSkillModel.addResult(playerAFacts.playerName, playerBFacts.playerName, playerAWinner)
  }
}