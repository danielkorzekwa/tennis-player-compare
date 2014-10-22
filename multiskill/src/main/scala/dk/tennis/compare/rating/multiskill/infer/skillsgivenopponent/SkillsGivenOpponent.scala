package dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill

case class SkillsGivenOpponent(
    skillsOnServeGivenOpponent:Map[String,Seq[PlayerSkill]],skillsOnReturnGivenOpponent:Map[String,Seq[PlayerSkill]])