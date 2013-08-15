package dk.tennis.compare.rating.trueskill.factorgraph

import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import scala.math._
import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

case class SingleGameFactorGraph(skill1: TrueSkillRating, skill2: TrueSkillRating, skillTransVariance: Double, player1PerfVar: Double,player2PerfVar:Double) {

  val skill1VarId_t0 = 1
  val skill1VarId = 2
  val skill2VarId_t0 = 3
  val skill2VarId = 4
  val perf1VarId = 5
  val perf2VarId = 6
  val perfDiffVarId = 7
  val outcomeVarId = 8

  def createTennisFactorGraph(): FactorGraph = {

    val skill1Factor_time0 = new GaussianFactor(skill1VarId_t0, skill1.mean, skill1.variance)
    val skill2Factor_time0 = new GaussianFactor(skill2VarId_t0, skill2.mean, skill2.variance)
    val skill1Factor = LinearGaussianFactor(skill1VarId_t0, skill1VarId, 1, 0, skillTransVariance)
    val skill2Factor = LinearGaussianFactor(skill2VarId_t0, skill2VarId, 1, 0, skillTransVariance)
    val perf1Factor = LinearGaussianFactor(skill1VarId, perf1VarId, 1, 0, player1PerfVar)
    val perf2Factor = LinearGaussianFactor(skill2VarId, perf2VarId, 1, 0, player2PerfVar)
    val perfDiffFactor = DiffGaussianFactor(perf1VarId, perf2VarId, perfDiffVarId)
    val outcomeFactor = TruncGaussianFactor(perfDiffVarId, outcomeVarId, 0)

    val factorGraph = GenericFactorGraph()

    factorGraph.addFactor(skill1Factor_time0)
    factorGraph.addFactor(skill2Factor_time0)
    factorGraph.addFactor(skill1Factor)
    factorGraph.addFactor(skill2Factor)
    factorGraph.addFactor(perf1Factor)
    factorGraph.addFactor(perf2Factor)
    factorGraph.addFactor(perfDiffFactor)
    factorGraph.addFactor(outcomeFactor)

    factorGraph
  }
  
}