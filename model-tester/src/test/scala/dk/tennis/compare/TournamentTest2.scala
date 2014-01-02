package dk.tennis.compare

import org.junit._
import org.junit.Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.util.Random
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennisprob.tournament.GenericTournamentProbCalc
import dk.tennis.compare.rating.multiskill.model.career.TournamentSkills
import dk.tennis.compare.rating.multiskill.model.career.GenericCareerModel
import dk.tennis.compare.rating.multiskill.model.career.CareerModelConfig
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel

class TournamentTest2 {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  val matchesFile = "./resources/tennis/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2006, 2013)

  val careerModelConfig = CareerModelConfig(
    initialSkillsOnServe = PlayerSkill(4002.542974700307, 0.8806472793221474), initialSkillsOnReturn = PlayerSkill(3997.9909513252546, 0.7527376376092434),
    skillOnServeTransVariance = 0.0005, skillOnReturnTransVariance = 0.0004,
    pointPerfVarianceOnServe = 102.61914136268837, pointPerfVarianceOnReturn = 102.61914136268837)

  @Test def test {
println("new")
    val tournamentSkills: Seq[TournamentSkills] = GenericCareerModel(careerModelConfig).calcTournamentSkills(tournaments)

    val playerRankings: Map[String, Double] = tournamentSkills.last.latestAllSkills.map {
      case (playerName, skills) =>

        (playerName, skills.skillOnServe.mean + skills.skillOnReturn.mean)
    }

    val topPlayers = playerRankings.toList.sortWith((r1, r2) => r1._2 > r2._2).take(128).map(r => r._1)
    val draw = calcDraw(topPlayers)

    println(winProb(tournamentSkills.last.latestAllSkills, "Novak Djokovic", "Rafael Nadal"))
    val tournamentProbs = GenericTournamentProbCalc.winningProbs(draw, winProb(tournamentSkills.last.latestAllSkills, _, _)).toList.sortWith((p1, p2) => p1._2 > p2._2)
    println(tournamentProbs)

  }

  private def calcDraw(players: Seq[String]): Seq[Tuple2[String, String]] = {

    val playersNum = players.size
    val seedsNum = 2
    val topTwoPlayerPosistion = List((players(0), 1), (players(1), playersNum))

    val remainingSeedsPos = Random.shuffle((2 to seedsNum - 1).map(i => i * (playersNum / i)))
    val remainingSeedsPlayersPos = players.drop(2).take(seedsNum - 2).zip(remainingSeedsPos)

    val otherPos = Random.shuffle((2 to playersNum - 1).diff(remainingSeedsPos))
    val otherPlayers = players.drop(seedsNum).zip(otherPos)

    val orderedPlayers = (remainingSeedsPlayersPos ++ otherPlayers ++ topTwoPlayerPosistion).sortBy(p => p._2).map(p => p._1)
    val draw = orderedPlayers.grouped(2).map(pair => (pair(0), pair(1))).toSeq
    draw
  }

  private def toGame(t: TournamentResult, r: MatchResult): Game = {
    val p1PointsOnServe = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
    val p2PointsOnServe = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
    val pointGame = Game(t.tournamentTime, r.player1, r.player2, p1PointsOnServe, p2PointsOnServe)
    pointGame
  }

  private def calcMatchProb(p1PointProb: Double, p2PointProb: Double, numOfSets: Int): Double = {

    val p1MatchProb = if (numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)
    p1MatchProb
  }

  def winProb(latestSkills: Map[String, PlayerSkills], player1: String, player2: String): Double = {

    val genericPointModel = GenericPointModel(195.61914136268837, 155)

    val player1Skills = latestSkills(player1)
    val player2Skills = latestSkills(player2)
    val p1PointProb = genericPointModel.pointProb(player1Skills.skillOnServe, player2Skills.skillOnReturn)
    val p2PointProb = genericPointModel.pointProb(player2Skills.skillOnServe, player1Skills.skillOnReturn)
    val matchProb = calcMatchProb(p1PointProb, p2PointProb, 3)
    matchProb

  }
}