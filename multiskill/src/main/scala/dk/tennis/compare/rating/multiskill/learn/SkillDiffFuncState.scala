package dk.tennis.compare.rating.multiskill.learn

import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

case class SkillDiffFuncState(params: DenseVector[Double])
    