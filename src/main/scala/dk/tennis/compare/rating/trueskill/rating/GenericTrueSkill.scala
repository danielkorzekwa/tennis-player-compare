package dk.tennis.compare.rating.trueskill.rating

import scala.collection._
import dk.bayes.gaussian.Gaussian
import dk.bayes.gaussian.LinearGaussian
import scala.math._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.bayes.gaussian.MultivariateGaussian.toGaussian
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.trueskill.factorgraph.SingleGameFactorGraph
import dk.bayes.infer.ep.GenericEP
import dk.tennis.compare.rating.trueskill.factorgraph.SingleGameFactorGraph
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.trueskill.model.Result

case class GenericTrueSkill(skillTransVariance: Double, performanceVariance: Double) extends TrueSkill {

  private val skillsMap: mutable.Map[String, TrueSkillRating] = mutable.Map()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addResult(result:Result) = {

    val player1Skill = skillsMap.getOrElse(result.player1, defaultSkill)
    val player2Skill = skillsMap.getOrElse(result.player2, defaultSkill)

    val (newPlayer1Skill, newPlayer2Skill) = computeMarginals(player1Skill, player2Skill, result.player1Win)
    skillsMap += result.player1 -> newPlayer1Skill
    skillsMap += result.player2 -> newPlayer2Skill
  }

  def getRatings(): immutable.Map[String, TrueSkillRating] = skillsMap.toMap

  private def computeMarginals(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating, player1Win: Boolean): Tuple2[TrueSkillRating, TrueSkillRating] = {

    val factorGraph = SingleGameFactorGraph(player1Skill, player2Skill, skillTransVariance, performanceVariance)

    val ep = GenericEP(factorGraph.createTennisFactorGraph())

    if (player1Win) ep.setEvidence(factorGraph.outcomeVarId, 0)
    else ep.setEvidence(factorGraph.outcomeVarId, 1)

    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
    ep.calibrate(100, progress)

    val s1Marginal = ep.marginal(factorGraph.skill1VarId).asInstanceOf[GaussianFactor]
    val s2Marginal = ep.marginal(factorGraph.skill2VarId).asInstanceOf[GaussianFactor]

    (TrueSkillRating(s1Marginal.m, s1Marginal.v), TrueSkillRating(s2Marginal.m, s2Marginal.v))
  }

}