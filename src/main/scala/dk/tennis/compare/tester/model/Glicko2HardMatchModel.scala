package dk.tennis.compare.tester.model

import dk.tennis.compare.rating.glicko2.GenericGlicko2Rating
import dk.tennis.compare.rating.glicko2.Glicko2Rating._
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.math._
import java.util._
import dk.atp.api.domain.MatchComposite
import dk.tennis.compare.tester.MatchModel

case class Glicko2HardMatchModel extends MatchModel {

  val glicko2 = new GenericGlicko2Rating(0, 100d / 173.7178, 0.03, 0.2, 3)

  def matchProb(m: MatchComposite): Option[Double] = {

    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts
    val matchTime = m.tournament.tournamentTime

    val ratings = glicko2.getRatings()
    val defaultRating = PlayerRating(Rating(0.5, Double.MaxValue, Double.MaxValue, matchTime), Rating(0.5, Double.MaxValue, Double.MaxValue, matchTime))
    val ratingA = ratings.getOrElse(playerAFacts.playerName, defaultRating)
    val ratingB = ratings.getOrElse(playerBFacts.playerName, defaultRating)

    val playerAOnServeProb = GenericGlicko2Rating.E(ratingA.ratingOnServe.rating, ratingB.ratingOnReturn.rating, ratingB.ratingOnReturn.deviation)
    val playerBOnServeProb = GenericGlicko2Rating.E(ratingB.ratingOnServe.rating, ratingA.ratingOnReturn.rating, ratingA.ratingOnReturn.deviation)

    if (!ratingA.ratingOnServe.rating.isNaN() &&
      !ratingB.ratingOnServe.rating.isNaN() &&
      !ratingA.ratingOnReturn.rating.isNaN() &&
      !ratingB.ratingOnReturn.rating.isNaN() &&
      ratingA.ratingOnServe.deviation < 0.6 &&
      ratingB.ratingOnServe.deviation < 0.6 &&
      ratingA.ratingOnReturn.deviation < 0.6 &&
      ratingB.ratingOnReturn.deviation < 0.6)
      Some(playerAOnServeProb) else None
  }

  def addMatchResult(m: MatchComposite) {
    val playerAFacts = m.matchFacts.playerAFacts
    val playerBFacts = m.matchFacts.playerBFacts

    val playerAWinner: Int = if (m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)) 1 else 0

    val result =
      Result(playerAFacts.playerName, playerBFacts.playerName, playerAWinner, m.tournament.tournamentTime)

    glicko2.sendResult(result)
  }
}