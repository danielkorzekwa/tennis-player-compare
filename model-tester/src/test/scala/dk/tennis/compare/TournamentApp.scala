package dk.tennis.compare

import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.tennisprob.tournament.GenericTournamentProbCalc
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults.InferMatchProbGivenMatchResults
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.matchloader.PlayerStats
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

object TournamentApp extends App with Logging {

  val df = new SimpleDateFormat("dd/MM/yyyy")
  val time = df.parse("18/11/2015")
  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2014_121114.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2014)

  logger.info("Building skills model...")
  val matchModel = InferMatchProbGivenMatchResults(matchResults.toIndexedSeq)
  logger.info("Building skills model...DONE")

  val players = getPlayers()
  players.foreach(println(_))

  val draw = getDraw(players.toIndexedSeq)
  draw.foreach(println(_))

  val winningProbs = GenericTournamentProbCalc.winningProbs(draw, matchProb)
  println(winningProbs.toList.sortBy(v => v._2).foreach(println(_)))

  def matchProb(player1: String, player2: String): Double = {
    val result = MatchResult(time, "tournament name", Surface.HARD, player1, player2, time, player1Won = true, numOfSets = 3, PlayerStats(0, 0, 0), PlayerStats(0, 0, 0))

    matchModel.predict(result).matchProb(player1)
  }

  private def getPlayers(): Seq[String] = {
    val allPlayers = matchResults.filter(m => m.tournamentTime.getTime() > df.parse("01/07/2014").getTime()).flatMap(r => List(r.player1, r.player2)).distinct

    val skills = allPlayers.map { p =>
      val playerOnServe = Player(p, p, true, time, Surface.HARD)
      val skillOnServe = matchModel.infer.inferSkill(playerOnServe)

      val playerOnReturn = Player(p, p, false, time, Surface.HARD)
      val skillOnReturn = matchModel.infer.inferSkill(playerOnReturn)

      (p, skillOnServe.skill.m + skillOnReturn.skill.m)
    }.sortWith((s1,s2) => s1._2>s2._2)

    skills.take(128).map(s => s._1)

  }

  private def getDraw(players: IndexedSeq[String]): Seq[Tuple2[String, String]] = {

    var groups: Array[Array[String]] = players.map(p => Array(p)).toArray

    while (groups.size > 1) {
      groups = (0 until groups.size / 2).map(i => groups(i) ++ groups(groups.size - i - 1)).toArray
    }

   groups(0).grouped(2).map(g => (g(0),g(1))).toList
  }
}