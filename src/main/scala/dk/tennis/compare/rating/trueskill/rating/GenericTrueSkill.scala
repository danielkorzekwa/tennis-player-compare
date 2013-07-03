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
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate

case class GenericTrueSkill(skillTransVariance: Double) extends TrueSkill {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  private val skillsMap: mutable.Map[String, TrueSkillRating] = mutable.Map()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addResult(result: Result, perfVariance: Tuple2[Double, Double]) = {

    val player1Skill = skillsMap.getOrElse(result.player1, defaultSkill)
    val player2Skill = skillsMap.getOrElse(result.player2, defaultSkill)

    val (newPlayer1Skill, newPlayer2Skill) = computeMarginals(player1Skill, player2Skill, result, perfVariance)
    skillsMap += result.player1 -> newPlayer1Skill
    skillsMap += result.player2 -> newPlayer2Skill
  }

  def getRatings(): immutable.Map[String, TrueSkillRating] = skillsMap.toMap

  private def computeMarginals(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating, result: Result, perfVariance: Tuple2[Double, Double]): Tuple2[TrueSkillRating, TrueSkillRating] = {

    val factorGraph = SingleGameFactorGraph(player1Skill, player2Skill, skillTransVariance,
      result.player1PerfVar.getOrElse(perfVariance._1), result.player2PerfVar.getOrElse(perfVariance._2))

    val ep = GenericEP(factorGraph.createTennisFactorGraph())

    if (result.player1Win) ep.setEvidence(factorGraph.outcomeVarId, true)
    else ep.setEvidence(factorGraph.outcomeVarId, false)

    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
    val epCalibrate = ForwardBackwardEPCalibrate(ep.factorGraph)
    val iterNum = epCalibrate.calibrate(100, progress)
    //logger.debug("Iter total: " + iterNum)

    val s1Marginal = ep.marginal(factorGraph.skill1VarId).asInstanceOf[GaussianFactor]
    val s2Marginal = ep.marginal(factorGraph.skill2VarId).asInstanceOf[GaussianFactor]

    (TrueSkillRating(s1Marginal.m, s1Marginal.v), TrueSkillRating(s2Marginal.m, s2Marginal.v))
  }

}