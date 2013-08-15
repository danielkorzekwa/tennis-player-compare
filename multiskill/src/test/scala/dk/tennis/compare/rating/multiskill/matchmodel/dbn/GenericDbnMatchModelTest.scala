package dk.tennis.compare.rating.multiskill.matchmodel.dbn

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.bayes.learn.lds.GenericLDSLearn
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.TransitionStat
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel

class GenericDbnMatchModelTest {

  private val pointP1Wins = List.fill(70)(PointResult("player1", true))
  private val pointP1Lose = List.fill(30)(PointResult("player1", false))

  private val pointP2Wins = List.fill(64)(PointResult("player2", true))
  private val pointP2Lose = List.fill(36)(PointResult("player2", false))
  private val pointResults = pointP1Wins ::: pointP1Lose ::: pointP2Wins ::: pointP2Lose

  private val matchResult = MatchResult("player1", "player2", pointResults, true, 2)

  private val p1Skills = PlayerSkills("player1", PlayerSkill(0.75, 0.1), PlayerSkill(0.13, 0.1))
  private val p2Skills = PlayerSkills("player2", PlayerSkill(0.46, 0.1), PlayerSkill(0.1, 0.1))

  private val perfVarianceOnServe = 2
  private val perfVarianceOnReturn = 1

  private val matchModel = GenericDbnMatchModel(p1Skills, p2Skills, perfVarianceOnServe, perfVarianceOnReturn, matchResult)

  @Test def mStep_for_perf_variance {

    val pointModel = GenericPointModel(perfVarianceOnServe, perfVarianceOnReturn)

    println("p1OnServePointProb=" + pointModel.pointProb(p1Skills.skillOnServe, p2Skills.skillOnReturn))
    println("p2OnServePointProb=" + pointModel.pointProb(p2Skills.skillOnServe, p1Skills.skillOnReturn))

    println("Calibration iter num=" + matchModel.calibrate(100))

    val perfOnServeStats = matchModel.getPerfVarOnServe().map(f => toTransitionStat(f))
    val newVarOnServe = GenericLDSLearn.newQ(perfOnServeStats)
    println("new perf var on serve=" + newVarOnServe)

    val perfOnReturnStats = matchModel.getPerfVarOnReturn().map(f => toTransitionStat(f))
    val newVarOnReturn = GenericLDSLearn.newQ(perfOnReturnStats)
    println("new perf var on return=" + newVarOnReturn)
  }

  private def toTransitionStat(factor: BivariateGaussianFactor): TransitionStat = {

    val transitionStat = TransitionStat(factor.mean(0), factor.variance(0, 0),
      factor.mean(1), factor.variance(1, 1), factor.variance(0, 1))

    transitionStat

  }
}