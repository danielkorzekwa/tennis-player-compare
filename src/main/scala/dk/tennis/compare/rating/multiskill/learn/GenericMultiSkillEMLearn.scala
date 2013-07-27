package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.LatentVariable
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

/**
 * Learns skill transition variance based on tennis point outcomes and skills on serve and return for both players.
 *
 * @author Daniel Korzekwa
 */
object GenericMultiSkillEMLearn extends MultiSkillEMLearn {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  def learn(multiSkillParams: MultiSkillParams, results: Seq[MatchResult], maxIter: Int, iterStatus: EMStatus => Unit = nilIterStatus): MultiSkillParams = {
    require(maxIter > 0, "Number of EM iterations is zero")

    @tailrec
    def learnRecursion(currMultiSkillParams: MultiSkillParams, currIter: Int): MultiSkillParams = {

      val loglik = calcLoglik(currMultiSkillParams, results)

      iterStatus(EMStatus(currIter, currMultiSkillParams, loglik))

      val tennisFactorGraph = TennisDbnFactorGraph(currMultiSkillParams)
      results.foreach(r => tennisFactorGraph.addTennisMatch(r))

      //E-step
      val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())

      val calibrateIterNum = epCalibrate.calibrate(1000, iterNum => {})
      logger.debug("EP calibration iterations: " + calibrateIterNum.iterNum)

      //M-step
      val newParams = mStep(tennisFactorGraph, currMultiSkillParams)

      if (currIter < maxIter) learnRecursion(newParams, currIter + 1)
      else newParams
    }

    val learnedParams = learnRecursion(multiSkillParams, 1)
    learnedParams

  }

  private def nilIterStatus(emStatus: EMStatus): Unit = {}

  /**
   * Returns maximised skill transition variance.
   *
   * @param em Calibrated factor graph
   */
  private def mStep(tennisFactorGraph: TennisDbnFactorGraph, currMultiSkillParams: MultiSkillParams): MultiSkillParams = {

    val skillOnServeVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnServe().values.filter(varIds => varIds.size >= 2)
    val skillsOnServeLatentVars: Iterable[Seq[LatentVariable]] = skillOnServeVarIds.map { varIds => toLatentVariables(varIds, tennisFactorGraph) }
    val newSkillOnServeTransVariance = GenericLDSLearn.newQ(skillsOnServeLatentVars.toSeq)

    val skillOnReturnVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnReturn().values.filter(varIds => varIds.size >= 2)
    val skillsOnReturnLatentVars: Iterable[Seq[LatentVariable]] = skillOnReturnVarIds.map { varIds => toLatentVariables(varIds, tennisFactorGraph) }
    val newSkillOnReturnTransVariance = GenericLDSLearn.newQ(skillsOnReturnLatentVars.toSeq)

    MultiSkillParams(newSkillOnServeTransVariance, newSkillOnReturnTransVariance,
      currMultiSkillParams.priorSkillOnServe, currMultiSkillParams.priorSkillOnServe,
      currMultiSkillParams.perfVariance)
  }

  private def toLatentVariables(skillVarIds: IndexedSeq[Int], tennisFactorGraph: TennisDbnFactorGraph): Seq[LatentVariable] = {
    require(skillVarIds.size >= 2, "Sequence of skill varIds must have at least two elements")

    val ep = GenericEP(tennisFactorGraph.getFactorGraph())

    val priorFactor = ep.marginal(skillVarIds.head).asInstanceOf[GaussianFactor]
    val priorVariable = LatentVariable(priorFactor.m, priorFactor.v, None)

    val transitionVariables = skillVarIds.sliding(2).map {
      case Seq(varT0, varT1) =>
        val factorT1 = ep.marginal(varT1).asInstanceOf[GaussianFactor]
        val transitionFactor = ep.marginal(varT0, varT1).asInstanceOf[BivariateGaussianFactor]
        val transitionLatentVariable = LatentVariable(factorT1.m, factorT1.v, Some(transitionFactor.variance.at(1)))
        transitionLatentVariable
    }.toList

    priorVariable :: transitionVariables
  }

  private def calcLoglik(multiSkillParams: MultiSkillParams, results: Seq[MatchResult]): Double = {
    val multiSkill = GenericMultiSkill(multiSkillParams)

    val loglik = results.foldLeft(0d) { (totalLogLik, r) =>

      val player1WinProb = calcPlayer1WinProb(r, multiSkill)
      multiSkill.processTennisMatch(r)

      val matchLogLik = if (r.player1Won) log(player1WinProb) else log1p(-player1WinProb)
      totalLogLik + matchLogLik
    }
    loglik
  }

  private def calcPlayer1WinProb(matchResult: MatchResult, multiSkill: GenericMultiSkill): Double = {

    val player1Skill = multiSkill.getSkill(matchResult.player1)
    val player2Skill = multiSkill.getSkill(matchResult.player2)

    val perfVariance = multiSkill.multiSkillParams.perfVariance

    val p1PointProb = GenericPointModel(perfVariance).pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    val p2PointProb = GenericPointModel(perfVariance).pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    val matchProb = if (matchResult.numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

    matchProb
  }
}