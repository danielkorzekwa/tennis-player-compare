package dk.tennis.compare.rating.multiskill.matchmodel.online

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import scala.collection.mutable.ArrayBuffer

case class GenericOnlineMatchModel(initialP1Skills: PlayerSkills, initialP2Skills: PlayerSkills, perfVarianceOnServe: Double, perfVarianceOnReturn: Double)
  extends OnlineMatchModel {

  private var p1SkillOnServe = initialP1Skills.skillOnServe
  private var p1SkillOnReturn = initialP1Skills.skillOnReturn

  private var p2SkillOnServe = initialP2Skills.skillOnServe
  private var p2SkillOnReturn = initialP2Skills.skillOnReturn

  private val pointModel = GenericPointModel(perfVarianceOnServe, perfVarianceOnReturn)

  def onPoint(pointResult: PointResult) {

    if (pointResult.playerOnServe.equals(initialP1Skills.player)) {

      val (newP1SkillOnServe, newP2SkillOnReturn) = pointModel.skillMarginals(p1SkillOnServe, p2SkillOnReturn, pointResult.playerOnServeWin)
      p1SkillOnServe = newP1SkillOnServe
      p2SkillOnReturn = newP2SkillOnReturn

    } else if (pointResult.playerOnServe.equals(initialP2Skills.player)) {

      val (newP2SkillOnServe, newP1SkillOnReturn) = pointModel.skillMarginals(p2SkillOnServe, p1SkillOnReturn, pointResult.playerOnServeWin)
      p2SkillOnServe = newP2SkillOnServe
      p1SkillOnReturn = newP1SkillOnReturn

    } else throw new IllegalArgumentException("Wrong player name: " + pointResult.playerOnServe)

  }

  def getP1Skills(): PlayerSkills = PlayerSkills(initialP1Skills.player, p1SkillOnServe, p1SkillOnReturn)
  def getP2Skills(): PlayerSkills = PlayerSkills(initialP2Skills.player, p2SkillOnServe, p2SkillOnReturn)

}