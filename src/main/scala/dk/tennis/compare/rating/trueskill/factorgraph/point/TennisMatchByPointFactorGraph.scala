package dk.tennis.compare.rating.trueskill.factorgraph.point

import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.bayes.model.factor.GaussianFactor
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor

object TennisMatchByPointFactorGraph {

  def create(player1Name: String, skill1: GaussianFactor, player2Name: String, skill2: GaussianFactor, playerPerfVar: Double, pointResults: Seq[Result]): FactorGraph = {
    val factorGraph = new GenericFactorGraph()

    factorGraph.addFactor(skill1)
    factorGraph.addFactor(skill2)

    val lastVarId = new AtomicInteger(List(skill1.varId, skill2.varId).max + 1)

    def addTennisPointToFactorGraph(player1VarId: Int, player2VarId: Int, player1Win: Boolean) {

      val perf1VarId = lastVarId.getAndIncrement()
      val perf2VarId = lastVarId.getAndIncrement()
      val perfDiffVarId = lastVarId.getAndIncrement()
      val outcomeVarId = lastVarId.getAndIncrement()

      factorGraph.addFactor(LinearGaussianFactor(player1VarId, perf1VarId, 1, 0, playerPerfVar))
      factorGraph.addFactor(LinearGaussianFactor(player2VarId, perf2VarId, 1, 0, playerPerfVar))
      factorGraph.addFactor(DiffGaussianFactor(perf1VarId, perf2VarId, perfDiffVarId))

      val outcomeFactor = TruncGaussianFactor(perfDiffVarId, outcomeVarId, 0)
      val outcomeFactorWithEvidence = if (player1Win) outcomeFactor.withEvidence(outcomeVarId, true)
      else outcomeFactor.withEvidence(outcomeVarId, false)
      factorGraph.addFactor(outcomeFactorWithEvidence)
    }

    pointResults.foreach { r =>

      if (r.player1.equals(player1Name))
        addTennisPointToFactorGraph(skill1.varId, skill2.varId, r.player1Win)
      else if (r.player1.equals(player2Name)) addTennisPointToFactorGraph(skill2.varId, skill1.varId, r.player1Win)
      else throw new IllegalArgumentException("Mismatch between point and match player names")
    }

    factorGraph
  }

}