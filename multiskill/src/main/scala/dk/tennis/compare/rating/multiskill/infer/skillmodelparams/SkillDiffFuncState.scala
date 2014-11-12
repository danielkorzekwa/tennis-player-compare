package dk.tennis.compare.rating.multiskill.infer.skillmodelparams
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.PerfDiff

case class SkillDiffFuncState(skillMeanFunc:(Player) => Double,skillCovFunc:CovFunc,logLik:Double,loglikD:Array[Double],perfDiffs:Array[PerfDiff])
    