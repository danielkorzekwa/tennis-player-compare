package dk.tennis.compare.rating.multiskill.matchmodel.online

import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

trait OnlineMatchModel {

  def onPoint(result: PointResult)

  def getP1Skills(): PlayerSkills
  def getP2Skills(): PlayerSkills
  
}