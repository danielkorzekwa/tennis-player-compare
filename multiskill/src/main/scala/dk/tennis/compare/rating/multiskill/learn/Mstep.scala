package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.bayes.infer.ep.GenericEP
import dk.bayes.learn.lds.TransitionStat
import dk.bayes.learn.lds.GenericLDSLearn
import dk.bayes.learn.lds.PriorStat
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.EP
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger

object Mstep {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  def maximise(tennisFactorGraph: TennisDbnFactorGraph, currMultiSkillParams: MultiSkillParams): MultiSkillParams = {
    val ep = GenericEP(tennisFactorGraph.getFactorGraph())

    //learn skill transition variance on serve
    val skillOnServeVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnServe().values.filter(varIds => varIds.size >= 50)
    val skillsOnServeStats: IndexedSeq[TransitionStat] = skillOnServeVarIds.flatMap(varIds => toTransitionStats(varIds, ep)).toIndexedSeq
    val newSkillOnServeTransVariance = GenericLDSLearn.newQ(skillsOnServeStats.toIndexedSeq)

    //learn skill transition variance on return
    val skillOnReturnVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnReturn().values.filter(varIds => varIds.size >= 50)
    val skillsOnReturnStats: IndexedSeq[TransitionStat] = skillOnReturnVarIds.flatMap(varIds => toTransitionStats(varIds, ep)).toIndexedSeq
    val newSkillOnReturnTransVariance = GenericLDSLearn.newQ(skillsOnReturnStats.toIndexedSeq)

    //learn prior skill on serve
    val priorSkillsOnServeVarIds: Iterable[Int] = tennisFactorGraph.getSkillVarIdsOnServe().values.filter(varIds => varIds.size >= 50).map(varIds => varIds.head)
    val priorSkillsOnServeStats: IndexedSeq[PriorStat] = priorSkillsOnServeVarIds.map(varId => toPriorStat(varId, ep)).toIndexedSeq
    val newPriorMeanOnServe = GenericLDSLearn.newPi(priorSkillsOnServeStats)
    val newPriorVarianceOnServe = GenericLDSLearn.newV(priorSkillsOnServeStats)
    val newPriorSkillOnServe = PlayerSkill(newPriorMeanOnServe, newPriorVarianceOnServe)

    //learn prior skill on return
    val priorSkillsOnReturnVarIds: Iterable[Int] = tennisFactorGraph.getSkillVarIdsOnReturn().values.filter(varIds => varIds.size >= 50).map(varIds => varIds.head)
    val priorSkillsOnReturnStats: IndexedSeq[PriorStat] = priorSkillsOnReturnVarIds.map(varId => toPriorStat(varId, ep)).toIndexedSeq
    val newPriorMeanOnReturn = GenericLDSLearn.newPi(priorSkillsOnReturnStats)
    val newPriorVarianceOnReturn = GenericLDSLearn.newV(priorSkillsOnReturnStats)
    val newPriorSkillOnReturn = PlayerSkill(newPriorMeanOnReturn, newPriorVarianceOnReturn)

    //learn performance variance on serve/return. Tuple2[perf stat on serve, perf stat on return]
    logger.info("Learning performance variance")
    val perfOnServeStats = tennisFactorGraph.getTennisMatchFactors.flatMap { tennisMatch =>

      val (perfMarginalsOnServe, perfMarginalsOnReturn) = tennisMatch.getPerfMarginals()
      perfMarginalsOnServe.map(perfMarginal => toTransitionStat(perfMarginal))
    }
    val newPerfVarOnServe = GenericLDSLearn.newQ(perfOnServeStats)

    val perfOnReturnStats = tennisFactorGraph.getTennisMatchFactors.flatMap { tennisMatch =>

      val (perfMarginalsOnServe, perfMarginalsOnReturn) = tennisMatch.getPerfMarginals()
      perfMarginalsOnReturn.map(perfMarginal => toTransitionStat(perfMarginal))
    }
    val newPerfVarOnReturn = GenericLDSLearn.newQ(perfOnReturnStats)

    MultiSkillParams(newSkillOnServeTransVariance, newSkillOnReturnTransVariance,
      newPriorSkillOnServe, newPriorSkillOnReturn,
      newPerfVarOnServe, newPerfVarOnReturn)
  }

  private def toTransitionStats(skillVarIds: IndexedSeq[Int], ep: EP): IndexedSeq[TransitionStat] = {
    require(skillVarIds.size >= 2, "Sequence of skill varIds must have at least two elements")

    val transitionStats = skillVarIds.sliding(2).map {
      case Seq(varT0, varT1) =>
        val transitionFactor = ep.marginal(varT0, varT1).asInstanceOf[BivariateGaussianFactor]
        val transitionStat = toTransitionStat(transitionFactor)
        transitionStat
    }.toList

    transitionStats.toIndexedSeq
  }

  private def toPriorStat(varId: Int, ep: EP): PriorStat = {
    val priorFactor = ep.marginal(varId).asInstanceOf[GaussianFactor]
    val priorVariable = PriorStat(priorFactor.m, priorFactor.v)
    priorVariable
  }

  private def toTransitionStat(factor: BivariateGaussianFactor): TransitionStat = {
    val transitionStat = TransitionStat(factor.mean(0), factor.variance(0, 0),
      factor.mean(1), factor.variance(1, 1), factor.variance(0, 1))
    transitionStat
  }

}