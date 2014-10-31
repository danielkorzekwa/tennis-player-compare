package dk.tennis.compare.rating.multiskill.learn

import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc

case class SkillDiffFuncState(skillCovFunc:CovFunc)
    