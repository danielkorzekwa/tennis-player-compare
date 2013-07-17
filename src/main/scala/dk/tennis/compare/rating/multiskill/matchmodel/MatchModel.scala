package dk.tennis.compare.rating.multiskill.matchmodel

import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait MatchModel {

  def onPoint(result: PointResult)

  def getP1Skills(): PlayerSkills
  def getP2Skills(): PlayerSkills
  
}