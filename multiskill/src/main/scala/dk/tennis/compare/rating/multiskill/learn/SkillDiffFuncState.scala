package dk.tennis.compare.rating.multiskill.learn

import breeze.linalg.DenseVector
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

case class SkillDiffFuncState(params: DenseVector[Double],
  currSkillPriorMeanOnServe: Double, currSkillPriorMeanOnReturn: Double,
  currSkillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], currSkillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]],

  newSkillPriorMeanOnServe: Double, newSkillPriorMeanOnReturn: Double,
  newSkillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], newSkillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]])
    