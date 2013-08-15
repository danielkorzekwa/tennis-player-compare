package dk.tennis.compare.rating.multiskill.sim

import scala.util.Random

import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel

case class GenericMatchSim(player1Skills: PlayerSkills, player2Skills: PlayerSkills, perfVarianceOnServe: Double, perfVarianceOnReturn: Double,
  pointsNum: Int, random: Random) extends MatchSim {

  private val pointModel = GenericPointModel(perfVarianceOnServe, perfVarianceOnReturn)
  private val p1OnServePointProb = pointModel.pointProb(player1Skills.skillOnServe, player2Skills.skillOnReturn)
  private val p2OnServePointProb = pointModel.pointProb(player2Skills.skillOnServe, player1Skills.skillOnReturn)

  def sample(): MatchResult = {

    val p1PointResults: List[PointResult] = List.fill(pointsNum)(samplePoint(player1Skills.player))
    val p2PointResults: List[PointResult] = List.fill(pointsNum)(samplePoint(player2Skills.player))

    val allPoints = p1PointResults ::: p2PointResults
    val player1Won = hasPlayer1Won(allPoints.last)

    val matchResult = MatchResult(player1Skills.player, player2Skills.player, allPoints, player1Won, numOfSets = 2)
    matchResult
  }

  private def samplePoint(playerOnServe: String): PointResult = {

    val pointProb = if (playerOnServe.equals(player1Skills.player)) p1OnServePointProb else p2OnServePointProb
    val won = random.nextDouble < pointProb
    val pointResult = PointResult(playerOnServe, won)
    pointResult
  }
  private def hasPlayer1Won(lastPoint: PointResult): Boolean = {
    if (lastPoint.playerOnServe.equals(player1Skills.player)) lastPoint.playerOnServeWin else !lastPoint.playerOnServeWin
  }
}