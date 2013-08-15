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
import dk.tennis.compare.rating.multiskill.matchmodel.dbn.DbnMatchModel
import dk.tennis.compare.rating.multiskill.matchmodel.dbn.GenericDbnMatchModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

object Mstep {

  def maximise(tennisFactorGraph: TennisDbnFactorGraph, currMultiSkillParams: MultiSkillParams): MultiSkillParams = {
    val ep = GenericEP(tennisFactorGraph.getFactorGraph())

    //learn skill transition variance on serve
    val skillOnServeVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnServe().values.filter(varIds => varIds.size >= 2)
    val skillsOnServeStats: IndexedSeq[TransitionStat] = skillOnServeVarIds.flatMap(varIds => toTransitionStats(varIds, ep)).toIndexedSeq
    val newSkillOnServeTransVariance = GenericLDSLearn.newQ(skillsOnServeStats.toIndexedSeq)

    //learn skill transition variance on return
    val skillOnReturnVarIds: Iterable[IndexedSeq[Int]] = tennisFactorGraph.getSkillVarIdsOnReturn().values.filter(varIds => varIds.size >= 2)
    val skillsOnReturnStats: IndexedSeq[TransitionStat] = skillOnReturnVarIds.flatMap(varIds => toTransitionStats(varIds, ep)).toIndexedSeq
    val newSkillOnReturnTransVariance = GenericLDSLearn.newQ(skillsOnReturnStats.toIndexedSeq)

    //learn prior skill on serve
    val priorSkillsOnServeVarIds: Iterable[Int] = tennisFactorGraph.getSkillVarIdsOnServe().values.map(varIds => varIds.head)
    val priorSkillsOnServeStats: IndexedSeq[PriorStat] = priorSkillsOnServeVarIds.map(varId => toPriorStat(varId, ep)).toIndexedSeq
    val newPriorMeanOnServe = GenericLDSLearn.newPi(priorSkillsOnServeStats)
    val newPriorVarianceOnServe = GenericLDSLearn.newV(priorSkillsOnServeStats)
    val newPriorSkillOnServe = PlayerSkill(newPriorMeanOnServe, newPriorVarianceOnServe)

    //learn prior skill on return
    val priorSkillsOnReturnVarIds: Iterable[Int] = tennisFactorGraph.getSkillVarIdsOnReturn().values.map(varIds => varIds.head)
    val priorSkillsOnReturnStats: IndexedSeq[PriorStat] = priorSkillsOnReturnVarIds.map(varId => toPriorStat(varId, ep)).toIndexedSeq
    val newPriorMeanOnReturn = GenericLDSLearn.newPi(priorSkillsOnReturnStats)
    val newPriorVarianceOnReturn = GenericLDSLearn.newV(priorSkillsOnReturnStats)
    val newPriorSkillOnReturn = PlayerSkill(newPriorMeanOnReturn, newPriorVarianceOnReturn)

    //learn performance variance on serve/return. Tuple2[perf stat on serve, perf stat on return]
    //    val tennisMatchModels: Seq[DbnMatchModel] = tennisFactorGraph.getTennisMatchFactors().map(f => toDbnMatchModel(f, currMultiSkillParams, ep))
    //    tennisMatchModels.foreach(m => m.calibrate(1000))
    //
    //    val perfOnServeStats: IndexedSeq[TransitionStat] = tennisMatchModels.flatMap(m => m.getPerfVarOnServe.map(f => toTransitionStat(f))).toIndexedSeq
    //    val newPerfVarOnServe = GenericLDSLearn.newQ(perfOnServeStats)
    //
    //    val perfOnReturnStats: IndexedSeq[TransitionStat] = tennisMatchModels.flatMap(m => m.getPerfVarOnReturn.map(f => toTransitionStat(f))).toIndexedSeq
    //    val newPerfVarOnReturn = GenericLDSLearn.newQ(perfOnReturnStats)

    MultiSkillParams(newSkillOnServeTransVariance, newSkillOnReturnTransVariance,
      newPriorSkillOnServe, newPriorSkillOnReturn,
      currMultiSkillParams.perfVarianceOnServe, currMultiSkillParams.perfVarianceOnReturn)
  }

  private def toDbnMatchModel(f: TennisMatchFactor, currMultiSkillParams: MultiSkillParams, ep: EP): DbnMatchModel = {
    val p1SkillOnServeMarginal = ep.marginal(f.p1Factor.skillOnServeVarId).asInstanceOf[GaussianFactor]
    val p1SkillOnReturnMarginal = ep.marginal(f.p1Factor.skillOnReturnVarId).asInstanceOf[GaussianFactor]
    val p1SkillOnServe = PlayerSkill(p1SkillOnServeMarginal.m, p1SkillOnServeMarginal.v)
    val p1SkillOnReturn = PlayerSkill(p1SkillOnReturnMarginal.m, p1SkillOnReturnMarginal.v)
    val initialP1Skills = PlayerSkills(f.matchResult.player1, p1SkillOnServe, p1SkillOnReturn)

    val p2SkillOnServeMarginal = ep.marginal(f.p2Factor.skillOnServeVarId).asInstanceOf[GaussianFactor]
    val p2SkillOnReturnMarginal = ep.marginal(f.p2Factor.skillOnReturnVarId).asInstanceOf[GaussianFactor]
    val p2SkillOnServe = PlayerSkill(p2SkillOnServeMarginal.m, p2SkillOnServeMarginal.v)
    val p2SkillOnReturn = PlayerSkill(p2SkillOnReturnMarginal.m, p2SkillOnReturnMarginal.v)
    val initialP2Skills = PlayerSkills(f.matchResult.player2, p2SkillOnServe, p2SkillOnReturn)

    val matchModel = GenericDbnMatchModel(initialP1Skills, initialP2Skills, currMultiSkillParams.perfVarianceOnServe, currMultiSkillParams.perfVarianceOnReturn, f.matchResult)

    matchModel
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