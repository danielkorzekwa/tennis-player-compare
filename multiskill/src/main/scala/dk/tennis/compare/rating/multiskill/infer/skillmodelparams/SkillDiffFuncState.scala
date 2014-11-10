package dk.tennis.compare.rating.multiskill.infer.skillmodelparams
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

case class SkillDiffFuncState(skillMeanFunc:(Player) => Double,skillCovFunc:CovFunc,logLik:Double,loglikD:Array[Double])
    