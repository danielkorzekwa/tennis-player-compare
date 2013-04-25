package dk.tennis.compare.rating.trueskill.ratingdbn

import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import scala.collection.mutable.ListBuffer
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.GenericEP
import dk.tennis.compare.rating.trueskill.factorgraph.TennisDbnFactorGraph
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating

case class GenericTrueSkillDbn(skillTransVariance: Double, performanceVariance: Double) extends TrueSkillDbn {

  private val results = new ListBuffer[Result]()

  def addResult(result: Result) = {
    results += result
  }

  def calcRatings(): Map[String, TrueSkillRating] = {

    val tennisFactorGraphBuilder = TennisDbnFactorGraph(skillTransVariance, performanceVariance)
    results.foreach(r => tennisFactorGraphBuilder.addResult(r))

    val factorGraph = tennisFactorGraphBuilder.getFactorGraph()

    val ep = GenericEP(factorGraph)
    def progress(currIter: Int) = println("EP iteration: " + currIter)
    ep.calibrate(100, progress)

    /**Map[playerName, variable id]*/
    val latestSkillsVarIds: Map[String, Int] = tennisFactorGraphBuilder.getLatestSkillVarIds()

    val skillsMap = latestSkillsVarIds.map {
      case (playerName, varId) =>
        val marginal = ep.marginal(varId).asInstanceOf[GaussianFactor]
        val skill = TrueSkillRating(marginal.m, marginal.v)
        (playerName, skill)
    }

    skillsMap
  }
}