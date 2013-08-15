package dk.tennis.compare.rating.multiskill

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.sim.GenericMatchSim
import dk.tennis.compare.rating.multiskill.sim.GenericMatchSim
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.util.Random

class GenericMultiSkillTest {

  private val p1Skills = PlayerSkills("player1", PlayerSkill(2.0, 1), PlayerSkill(-2, 1))
  private val p2Skills = PlayerSkills("player2", PlayerSkill(5, 1), PlayerSkill(-3, 1))

  private val matchSim = GenericMatchSim(p1Skills, p2Skills, perfVarianceOnServe = 100, perfVarianceOnReturn = 100,
    random = new Random(), pointsNum = 100)

  val multiSkillParams = MultiSkillParams(
    skillOnServeTransVariance = 0.026103154972190633,
    skillOnReturnTransVariance = 0.06103154972158028,
    priorSkillOnServe = PlayerSkill(2.7100608379747073, 0.499569787325858), priorSkillOnReturn = PlayerSkill(-2.7100608379751296, 0.49956978732869395),
    perfVarianceOnServe = 100, perfVarianceOnReturn = 100)

  val multiSkill = GenericMultiSkill(multiSkillParams)
  val pointModel = GenericPointModel(multiSkillParams.perfVarianceOnServe, multiSkillParams.perfVarianceOnReturn)

  @Test def converge_two_players {

    for (i <- 1 to 100) multiSkill.processTennisMatch(matchSim.sample())

    printReport()

    multiSkill.processTennisMatch(matchSim.sample())

    printReport()
  }

  private def printReport() {
    val p1Skills = multiSkill.getSkill("player1")
    val p2Skills = multiSkill.getSkill("player2")

    val p1PointProb = pointModel.pointProb(p1Skills.skillOnServe, p2Skills.skillOnReturn)
    val p2PointProb = pointModel.pointProb(p2Skills.skillOnServe, p1Skills.skillOnReturn)

    println("p1 point prob=" + p1PointProb)
    println("p2 point prob=" + p2PointProb)
    println("p1 skills = " + p1Skills)
    println("p2 skills = " + p2Skills)
  }
}