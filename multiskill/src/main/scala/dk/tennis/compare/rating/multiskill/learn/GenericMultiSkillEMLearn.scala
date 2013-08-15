package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.GenericLDSLearn
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.GenericMultiSkill
import scala.math._
import dk.tennis.compare.rating.multiskill.MultiSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.infer.ep.EP
import dk.bayes.learn.lds.TransitionStat
import dk.bayes.learn.lds.PriorStat

/**
 * Learns skill transition variance based on tennis point outcomes and skills on serve and return for both players.
 *
 * @author Daniel Korzekwa
 */
object GenericMultiSkillEMLearn extends MultiSkillEMLearn {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  private def nilIterStatus(emStatus: EMStatus): Unit = {}
  
  def learn(multiSkillParams: MultiSkillParams, results: Seq[MatchResult], maxIter: Int, iterStatus: EMStatus => Unit = nilIterStatus): MultiSkillParams = {
    require(maxIter > 0, "Number of EM iterations is zero")

    @tailrec
    def learnRecursion(currMultiSkillParams: MultiSkillParams, currIter: Int): MultiSkillParams = {

      val loglikMatch = calcLoglikMatch(currMultiSkillParams, results)
      val loglikPoint = calcLoglikPoint(currMultiSkillParams, results)

      iterStatus(EMStatus(currIter, currMultiSkillParams, loglikMatch, loglikPoint))

      val tennisFactorGraph = TennisDbnFactorGraph(currMultiSkillParams)
      results.foreach(r => tennisFactorGraph.addTennisMatch(r))

      //E-step
      val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())

      val calibrateIterNum = epCalibrate.calibrate(1000, iterNum => {})
      logger.debug("EP calibration iterations: " + calibrateIterNum.iterNum)

      //M-step
      val newParams = Mstep.maximise(tennisFactorGraph, currMultiSkillParams)

      if (currIter < maxIter) learnRecursion(newParams, currIter + 1)
      else newParams
    }

    val learnedParams = if (results.isEmpty) multiSkillParams else learnRecursion(multiSkillParams, 1)
    learnedParams

  }

  private def calcLoglikMatch(multiSkillParams: MultiSkillParams, results: Seq[MatchResult]): Double = {
    val multiSkill = GenericMultiSkill(multiSkillParams)

    val loglik = results.foldLeft(0d) { (totalLogLik, r) =>

      val player1WinProb = calcPlayer1WinMatchProb(r, multiSkill)
      val matchLogLik = MatchLogLik.logLik(player1WinProb, r.player1Won)

      multiSkill.processTennisMatch(r)

      totalLogLik + matchLogLik
    }
    loglik
  }

  private def calcLoglikPoint(multiSkillParams: MultiSkillParams, results: Seq[MatchResult]): Double = {
    val multiSkill = GenericMultiSkill(multiSkillParams)

    val loglik = results.foldLeft(0d) { (totalLogLik, r) =>

      val (p1PointProb, p2PointProb) = calcPointProbs(r, multiSkill)
      val matchLogLik = MatchLogLik.logLikByPoint(p1PointProb, p2PointProb, r)

      multiSkill.processTennisMatch(r)

      totalLogLik + matchLogLik
    }
    loglik
  }

  /**Returns [p1PointOnServeProb,p2PointOnServeProb]*/
  private def calcPointProbs(matchResult: MatchResult, multiSkill: GenericMultiSkill): Tuple2[Double, Double] = {

    val player1Skill = multiSkill.getSkill(matchResult.player1)
    val player2Skill = multiSkill.getSkill(matchResult.player2)

    val pointModel = GenericPointModel(multiSkill.multiSkillParams.perfVarianceOnServe, multiSkill.multiSkillParams.perfVarianceOnReturn)

    val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    (p1PointProb, p2PointProb)
  }

  /**Returns player 1 match win probability*/
  private def calcPlayer1WinMatchProb(matchResult: MatchResult, multiSkill: GenericMultiSkill): Double = {

    val (p1PointProb, p2PointProb) = calcPointProbs(matchResult, multiSkill)

    val matchProb = if (matchResult.numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

    matchProb
  }

}