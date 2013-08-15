package dk.tennis.compare.rating.multiskill.matchmodel.dbn

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factor.GaussianFactor
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import scala.collection.mutable.ArrayBuffer
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.DiffGaussianFactor
import dk.bayes.model.factor.TruncGaussianFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

case class GenericDbnMatchModel(
  initialP1Skills: PlayerSkills, initialP2Skills: PlayerSkills, perfVarianceOnServe: Double, perfVarianceOnReturn: Double, matchResult: MatchResult) extends DbnMatchModel {

  require(initialP1Skills.player.equals(matchResult.player1), "Player name is inconsistent")
  require(initialP2Skills.player.equals(matchResult.player2), "Player name is inconsistent")

  private val varId = new AtomicInteger(0)

  private val p1SkillOnServe = GaussianFactor(varId.getAndIncrement(), initialP1Skills.skillOnServe.mean, initialP1Skills.skillOnServe.variance)
  private val p1SkillOnReturn = GaussianFactor(varId.getAndIncrement(), initialP1Skills.skillOnReturn.mean, initialP1Skills.skillOnReturn.variance)

  private val p2SkillOnServe = GaussianFactor(varId.getAndIncrement(), initialP2Skills.skillOnServe.mean, initialP2Skills.skillOnServe.variance)
  private val p2SkillOnReturn = GaussianFactor(varId.getAndIncrement(), initialP2Skills.skillOnReturn.mean, initialP2Skills.skillOnReturn.variance)

  private val factorGraph = GenericFactorGraph()
  private val ep = GenericEP(factorGraph)
  private val epCalibrate = new ForwardBackwardEPCalibrate(factorGraph)

  //Tuple2[skillVarId,perfVarId]
  private val perfVarIdOnServe: ArrayBuffer[Tuple2[Int, Int]] = ArrayBuffer()
  private val perfVarIdOnReturn: ArrayBuffer[Tuple2[Int, Int]] = ArrayBuffer()

  addSkillToFactorGraph()
  matchResult.pointResults.foreach(p => addPointToFactorGraph(p))

  def calibrate(maxIter: Int): Int = epCalibrate.calibrate(maxIter, iterNum => {}).iterNum

  def getPerfVarOnServe(): IndexedSeq[BivariateGaussianFactor] = {
    val perfOnServe = perfVarIdOnServe.map { case (skillVarId, perfVarId) => ep.marginal(skillVarId, perfVarId).asInstanceOf[BivariateGaussianFactor] }

    perfOnServe
  }
  def getPerfVarOnReturn(): IndexedSeq[BivariateGaussianFactor] = {
    val perfOnReturn = perfVarIdOnReturn.map { case (skillVarId, perfVarId) => ep.marginal(skillVarId, perfVarId).asInstanceOf[BivariateGaussianFactor] }

    perfOnReturn
  }

  def getP1Skills(): PlayerSkills = {
    val p1OnServeMarginal = ep.marginal(p1SkillOnServe.varId).asInstanceOf[GaussianFactor]
    val p1OnReturnMarginal = ep.marginal(p1SkillOnReturn.varId).asInstanceOf[GaussianFactor]

    PlayerSkills(initialP1Skills.player, PlayerSkill(p1OnServeMarginal.m, p1OnServeMarginal.v), PlayerSkill(p1OnReturnMarginal.m, p1OnReturnMarginal.v))
  }
  def getP2Skills(): PlayerSkills = {
    val p2OnServeMarginal = ep.marginal(p2SkillOnServe.varId).asInstanceOf[GaussianFactor]
    val p2OnReturnMarginal = ep.marginal(p2SkillOnReturn.varId).asInstanceOf[GaussianFactor]

    PlayerSkills(initialP2Skills.player, PlayerSkill(p2OnServeMarginal.m, p2OnServeMarginal.v), PlayerSkill(p2OnReturnMarginal.m, p2OnReturnMarginal.v))
  }

  private def addSkillToFactorGraph() {

    factorGraph.addFactor(p1SkillOnServe)
    factorGraph.addFactor(p1SkillOnReturn)
    factorGraph.addFactor(p2SkillOnServe)
    factorGraph.addFactor(p2SkillOnReturn)
  }

  private def addPointToFactorGraph(pointResult: PointResult) {

    val (playerOnServeVarId, playerOnReturnVarId) = if (pointResult.playerOnServe.equals(initialP1Skills.player))
      (p1SkillOnServe.varId, p2SkillOnReturn.varId) else (p2SkillOnServe.varId, p1SkillOnReturn.varId)

    val perfOnServeVarId = varId.getAndIncrement()
    val perfOnReturnVarId = varId.getAndIncrement()
    val perfDiffVarId = varId.getAndIncrement()
    val pointVarId = varId.getAndIncrement()

    factorGraph.addFactor(LinearGaussianFactor(playerOnServeVarId, perfOnServeVarId, 1, 0, perfVarianceOnServe))
    factorGraph.addFactor(LinearGaussianFactor(playerOnReturnVarId, perfOnReturnVarId, 1, 0, perfVarianceOnReturn))
    factorGraph.addFactor(DiffGaussianFactor(perfOnServeVarId, perfOnReturnVarId, perfDiffVarId))
    factorGraph.addFactor(TruncGaussianFactor(perfDiffVarId, pointVarId, 0, Some(pointResult.playerOnServeWin)))

    perfVarIdOnServe += Tuple2(playerOnServeVarId, perfOnServeVarId)
    perfVarIdOnReturn += Tuple2(playerOnReturnVarId, perfOnReturnVarId)

  }
}