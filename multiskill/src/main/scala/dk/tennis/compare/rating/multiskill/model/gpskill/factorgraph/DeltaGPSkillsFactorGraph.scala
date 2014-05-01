package dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player

case class DeltaGPSkillsFactorGraph(priorPlayerSkills: MultivariateGaussian, players: Array[Player],
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) {

  private var skillsMarginal: MultivariateGaussian = priorPlayerSkills

  val precision = Matrix(2, 2, Array.fill(4)(0d))
  val priorGameToSkillsFactorMsg = new CanonicalGaussian(
    precision, precision * Matrix.zeros(2, 1),
    Double.NaN)

  private var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = (1 to players.size / 2).map { gameFactor =>

    priorGameToSkillsFactorMsg
  }

  def sendMsgs() {
    //compute gameToSkillsFactorMsgs
    gameToSkillsFactorMsgs = gameToSkillsFactorMsgs.zipWithIndex.map {
      case (gameToSkillsMsg, index) =>

        skillsMarginal = updateMarginal2(skillsMarginal, gameToSkillsMsg, index * 2, index * 2+1, add = false)

        val (mean, variance) = (skillsMarginal.m, skillsMarginal.v)
        val directSkillsMean = Matrix(mean(index * 2), mean(index * 2+1))
        val var00 = variance(index * 2, index * 2)
        val var01 = variance(index * 2, index * 2+1)
        val var10 = variance(index * 2+1, index * 2)
        val var11 = variance(index * 2+1,index * 2+1)
        val directSkillsVariance = Matrix(2, 2, Array(var00, var01, var10, var11))

        val skillsToGameMsg = CanonicalGaussian(directSkillsMean, directSkillsVariance)

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

           val p1PointsWon = players(index * 2).pointsWon
        val p2PointsWon = players(index * 2 + 1).pointsWon
        val newDirectSkills = model.skillMarginals(skillsToGameMsg, p1PointsWon, p1PointsWon+p2PointsWon, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        if (!directSkillsMsg.mean(0).isNaN()) {
          skillsMarginal = updateMarginal2(skillsMarginal, directSkillsMsg, index * 2, index * 2+1, add = true)
          directSkillsMsg
        } else {
          skillsMarginal = updateMarginal2(skillsMarginal, gameToSkillsMsg, index * 2, index * 2+1, add = true)
          gameToSkillsMsg
        }
      }
    
  }

  private def updateMarginal1(marginal: MultivariateGaussian, gameMsg: CanonicalGaussian, p1Index: Int, p2Index: Int, add: Boolean): MultivariateGaussian = {
    val canon = CanonicalGaussian(marginal.m, marginal.v)
    val copy = new CanonicalGaussian(canon.k.copy(), canon.h.copy(), g = canon.g)

    val mult = if (add) 1 else -1
    val k00 = copy.k(p1Index, p1Index)
    copy.k.set(p1Index, p1Index, k00 + mult * gameMsg.k(0, 0))

    val k01 = copy.k(p1Index, p2Index)
    copy.k.set(p1Index, p2Index, k01 + mult * gameMsg.k(0, 1))

    val k10 = copy.k(p2Index, p1Index)
    copy.k.set(p2Index, p1Index, k10 + mult * gameMsg.k(1, 0))

    val k11 = copy.k(p2Index, p2Index)
    copy.k.set(p2Index, p2Index, k11 + mult * gameMsg.k(1, 1))

    val h0 = copy.h(p1Index)
    copy.h.set(p1Index, 0, h0 + mult * gameMsg.h(0))

    val h1 = copy.h(p2Index)
    copy.h.set(p2Index, 0, h1 + mult * gameMsg.h(1))

    MultivariateGaussian(copy.mean, copy.variance)

  }

  private def updateMarginal2(marginal: MultivariateGaussian, gameMsg: CanonicalGaussian, p1Index: Int, p2Index: Int, add: Boolean): MultivariateGaussian = {

    val mult = if (add) 1d else -1d

    val (u, w, v, rank) = gameMsg.k.svd()

    if (rank == 0) {
      marginal
    } else {
      require(rank == 1, "Only rank-1 update is supported")

      val uExt = Matrix.zeros(marginal.v.numRows, 1)
      uExt.set(p1Index, 0, u(0, 0))
      uExt.set(p2Index, 0, u(1, 0))

      val vExt = Matrix.zeros(marginal.v.numRows, 1)
      vExt.set(p1Index, 0, v(0, 0))
      vExt.set(p2Index, 0, v(1, 0))

      val newV = woodbury(marginal.v, mult * uExt, w.extractMatrix(0, 1, 0, 1).inv, vExt.t)

      val mExt = Matrix.zeros(marginal.v.numRows, 1)
      mExt.set(p1Index, 0, gameMsg.mean(0))
      mExt.set(p2Index, 0, gameMsg.mean(1))

      val newM = marginal.m + (-mult) * (newV * uExt) * (w.extractMatrix(0, 1, 0, 1) * vExt.t * (marginal.m - mExt))

      MultivariateGaussian(newM, newV)

    }

  }

  def getPlayerSkillsMarginal(): MultivariateGaussian = skillsMarginal

}
