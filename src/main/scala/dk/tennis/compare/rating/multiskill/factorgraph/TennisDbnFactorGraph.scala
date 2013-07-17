package dk.tennis.compare.rating.multiskill.factorgraph

import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.model.factorgraph.FactorGraph
import scala.collection._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor
import dk.tennis.compare.rating.multiskill.domain.PointResult
import scala.collection.mutable.ArrayBuffer
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.TennisMatchFactor2

case class TennisDbnFactorGraph(skillTransVariance: Double, perfVariance: Double) {

  private val factorGraph = GenericFactorGraph()

  private val skillOnServeVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()
  private val skillOnReturnVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

  /**key - playerName,value - temporal sequence of player skill variables*/
  private val skillVarIds: mutable.Map[String, ArrayBuffer[Int]] = mutable.Map[String, ArrayBuffer[Int]]()

  private val lastVarId = new AtomicInteger()

  private val defaultSkill = TrueSkillRating(0, 1)

  def addTennisMatch(player1: String, player2: String, pointResults: Seq[PointResult]) {

//    val player1OnServeVarId = getSkillTransitionVarId(player1, skillOnServeVarIds)
//    val player1OnReturnVarId = getSkillTransitionVarId(player1, skillOnReturnVarIds)
//
//    val player2OnServeVarId = getSkillTransitionVarId(player2, skillOnServeVarIds)
//    val player2OnReturnVarId = getSkillTransitionVarId(player2, skillOnReturnVarIds)
//
//    addTennisMatchToFactorGraph(player1OnServeVarId, player1OnReturnVarId,
//      player2OnServeVarId, player2OnReturnVarId, player1, player2, pointResults)

        val skill1VarId = getSkillTransitionVarId(player1, skillVarIds)
        val skill2VarId = getSkillTransitionVarId(player2, skillVarIds)
        addTennisMatchToFactorGraph(player1, skill1VarId, skill2VarId, pointResults)
  }

  private def getSkillTransitionVarId(player: String, skillsVarIds: mutable.Map[String, ArrayBuffer[Int]]): Int = {
    val playerSkillVars = skillsVarIds.getOrElseUpdate(player, {
      val gaussianFactor = new GaussianFactor(lastVarId.getAndIncrement(), defaultSkill.mean, defaultSkill.variance)
      factorGraph.addFactor(gaussianFactor)
      ArrayBuffer(gaussianFactor.varId)
    })

    val linearGaussianFactor = LinearGaussianFactor(playerSkillVars.last, lastVarId.getAndIncrement(), 1, 0, skillTransVariance)
    factorGraph.addFactor(linearGaussianFactor)

    playerSkillVars += linearGaussianFactor.varId

    linearGaussianFactor.varId
  }

  private def addTennisMatchToFactorGraph(player1: String, skill1VarId: Int, skill2VarId: Int, pointResults: Seq[PointResult]) {

    val p1Wins = pointResults.map { r =>
      if (r.playerOnServe.equals(player1)) r.playerOnServeWin else !r.playerOnServeWin
    }
    val outcomeVarId = lastVarId.getAndIncrement()
    val tennisMatchFactor = TennisMatchFactor(skill1VarId, skill2VarId, outcomeVarId, perfVariance, p1Wins.toVector)
    factorGraph.addFactor(tennisMatchFactor)

  }

  private def addTennisMatchToFactorGraph(player1OnServeVarId: Int, player1OnReturnVarId: Int,
    player2OnServeVarId: Int, player2OnReturnVarId: Int, player1: String, player2: String, pointResults: Seq[PointResult]) {

    val player1VarId = lastVarId.getAndIncrement()
    val player2VarId = lastVarId.getAndIncrement()
    val outcomeVarId = lastVarId.getAndIncrement()

    val player1Factor = PlayerFactor(player1OnServeVarId, player1OnReturnVarId, player1VarId)
    val player2Factor = PlayerFactor(player2OnServeVarId, player2OnReturnVarId, player2VarId)

    val matchFactor = TennisMatchFactor2(player1VarId, player2VarId, outcomeVarId, perfVariance, player1, player2, pointResults)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)
  }

   def getSkillVarIdsOnServe(): mutable.Map[String, ArrayBuffer[Int]] = skillOnServeVarIds
  def getSkillVarIdsOnReturn(): mutable.Map[String, ArrayBuffer[Int]] = skillOnReturnVarIds
  def getSkillVarIds(): mutable.Map[String, ArrayBuffer[Int]] = skillVarIds

  def getFactorGraph(): FactorGraph = factorGraph
}