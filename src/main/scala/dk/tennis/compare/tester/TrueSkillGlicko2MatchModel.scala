package dk.tennis.compare.tester

import dk.atp.api.domain.MatchComposite

case class TrueSkillGlicko2MatchModel extends MatchModel {

  private val glicko2Model = Glicko2MatchModel()
  private val trueSkillModel = TrueSkillMatchModel()

  def matchProb(m: MatchComposite): Option[Double] = {
    val glicko2Prob = glicko2Model.matchProb(m)
    val trueSkillProb = trueSkillModel.matchProb(m)
    
    if(glicko2Prob.isDefined && trueSkillProb.isDefined)
      Some(0.5 * glicko2Prob.get + 0.5 * trueSkillProb.get)
      else None
  }

  def addMatchResult(m: MatchComposite) {
    glicko2Model.addMatchResult(m)
    trueSkillModel.addMatchResult(m)
  }
}