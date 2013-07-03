package dk.tennis.compare.rating.trueskill.rating.servereturn

import dk.tennis.compare.rating.trueskill.model.Result
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import scala.collection._
import dk.tennis.compare.rating.trueskill.factorgraph.SingleGameFactorGraph
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate

case class GenericTrueSkillServeReturn(skillTransVariance: Double) extends TrueSkillServeReturn {

  /** Map[playerName,Tuple2[playerSkillServe,playerSkillReturn]]*/
  private val skillsMap: mutable.Map[String, Tuple2[TrueSkillRating, TrueSkillRating]] = mutable.Map()

  private val defaultSkill = Tuple2(TrueSkillRating(0, 1), TrueSkillRating(0, 1))

  def addResult(result: Result, perfVariance: Tuple2[Double, Double]) {

    val player1Skills = skillsMap.getOrElseUpdate(result.player1, defaultSkill)
    val player2Skills = skillsMap.getOrElseUpdate(result.player2, defaultSkill)

    val (newP1SkillOnServe, newP2SkillOnReturn) = computeMarginals(player1Skills._1, player2Skills._2, result, perfVariance)

    skillsMap += result.player1 -> (newP1SkillOnServe, player1Skills._2)
    skillsMap += result.player2 -> (player2Skills._1, newP2SkillOnReturn)

  }

  private def computeMarginals(player1Skill: TrueSkillRating, player2Skill: TrueSkillRating, result: Result, perfVariance: Tuple2[Double, Double]): Tuple2[TrueSkillRating, TrueSkillRating] = {

    val factorGraph = SingleGameFactorGraph(player1Skill, player2Skill, skillTransVariance,
      result.player1PerfVar.getOrElse(perfVariance._1), result.player2PerfVar.getOrElse(perfVariance._2))

    val ep = GenericEP(factorGraph.createTennisFactorGraph())

    if (result.player1Win) ep.setEvidence(factorGraph.outcomeVarId, 0)
    else ep.setEvidence(factorGraph.outcomeVarId, 1)

    def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
    val epCalibrate = ForwardBackwardEPCalibrate(ep.factorGraph)
    epCalibrate.calibrate(100, progress)

    val s1Marginal = ep.marginal(factorGraph.skill1VarId).asInstanceOf[GaussianFactor]
    val s2Marginal = ep.marginal(factorGraph.skill2VarId).asInstanceOf[GaussianFactor]

    (TrueSkillRating(s1Marginal.m, s1Marginal.v), TrueSkillRating(s2Marginal.m, s2Marginal.v))
  }

  def getRatings(): immutable.Map[String, Tuple2[TrueSkillRating, TrueSkillRating]] = skillsMap.toMap
}