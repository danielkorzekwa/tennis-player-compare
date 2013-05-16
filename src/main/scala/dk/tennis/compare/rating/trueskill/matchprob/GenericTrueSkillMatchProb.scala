package dk.tennis.compare.rating.trueskill.matchprob

import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.trueskill.factorgraph.SingleGameFactorGraph

case class GenericTrueSkillMatchProb(skillTransVariance: Double) extends TrueSkillMatchProb {

  def matchProb(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating,perfVariance: Tuple2[Double, Double]): Double = {
    val factorGraph = SingleGameFactorGraph(player1Skill, player2Skill, skillTransVariance, perfVariance._1,perfVariance._2)
    val ep = GenericEP(factorGraph.createTennisFactorGraph)

    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
    ep.calibrate(100, progress)

    val outcomeMarginal = ep.marginal(factorGraph.outcomeVarId).getValue((factorGraph.outcomeVarId, 0))
    outcomeMarginal
  }

}