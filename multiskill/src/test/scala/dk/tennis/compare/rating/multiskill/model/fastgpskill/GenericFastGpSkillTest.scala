package dk.tennis.compare.rating.multiskill.model.fastgpskill

import org.junit._
import Assert._
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel

class GenericFastGpSkillTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(1)

  val pointPerfVarianceOnServe = 102.61914136268837
  val pointPerfVarianceOnReturn = 102.61914136268837

  @Test def test {

    val players: Array[Player] = toPlayers(tournaments)
    logger.info(s"All players: ${players.size}")

    logger.info("Computing skills marginals")
    val infer = GenericFastGpSkill(pointPerfVarianceOnServe, pointPerfVarianceOnReturn)
    val skillMarginals = infer.skillMarginals(players)

    logger.info("Calculating log likeligood")

    val pointProbsOnServe: Seq[Double] = toPointProbs(skillMarginals)

    //Tuple2(pointsWon,pointsTotal)
    val pointStatsOnServe: Seq[Tuple2[Int, Int]] = toPointStatsOnServe(players)

    //(pointProb,pointsWon,pointsTotal)
    val predictions: Seq[Tuple3[Double, Int, Int]] = pointProbsOnServe.zip(pointStatsOnServe).map { case (prob, (pointsWon, pointsTotal)) => (prob, pointsWon, pointsTotal) }

    println("Log lik(totalLik,avgLik,pointsTotal): " + LogLik.logLik(predictions))
  }

  private def toPointProbs(skillMarginals: Seq[Gaussian]): Seq[Double] = {
    val (pointPerfVarianceOnServe, pointPerfVarianceOnReturn) = (195.61914136268837, 155)

    val pointModel = GenericPointModel(pointPerfVarianceOnServe, pointPerfVarianceOnReturn)
    val pointProbs = skillMarginals.grouped(2).map { case Seq(skill1, skill2) => pointModel.pointProb(skill1, skill2) }
    pointProbs.toList
  }

  private def toPointStatsOnServe(players: Seq[Player]): Seq[Tuple2[Int, Int]] = {

    val pointStatsOnServe = players.grouped(2).map {
      case Seq(pOnServe, pOnReturn) =>
        (pOnServe.pointsWon, pOnServe.pointsWon + pOnReturn.pointsWon)
    }

    pointStatsOnServe.toList
  }

  private def toPlayers(tournaments: Seq[TournamentResult]): Array[Player] = {

    val players = tournaments.flatMap { t =>

      t.matchResults.flatMap { r =>
        val player1OnServe = Player(r.player1, r.matchTime, r.p1Stats.servicePointsWon)
        val player2OnReturn = Player(r.player2, r.matchTime, r.p1Stats.servicePointsTotal - r.p1Stats.servicePointsWon)

        val player2OnServe = Player(r.player2, r.matchTime, r.p2Stats.servicePointsWon)
        val player1OnReturn = Player(r.player1, r.matchTime, r.p2Stats.servicePointsTotal - r.p2Stats.servicePointsWon)

        Array(player1OnServe, player2OnReturn, player2OnServe, player1OnReturn)
      }
    }

    players.toArray
  }
}