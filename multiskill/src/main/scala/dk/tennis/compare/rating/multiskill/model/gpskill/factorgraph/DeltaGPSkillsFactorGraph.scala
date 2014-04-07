package dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import DeltaGPSkillsFactorGraph._
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian

case class DeltaGPSkillsFactorGraph(priorPlayerSkills: GPSkills,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) {

  private val directSkillsFactors: Seq[GameFactor] = GameFactor.toGameFactors(Game.toGames(priorPlayerSkills.matchResults), priorPlayerSkills)

  private var skillsMarginal: MultivariateGaussian = priorPlayerSkills.skillsGaussian

  val precision = Matrix(2, 2, Array.fill(4)(0d))
  val priorGameToSkillsFactorMsg = new CanonicalGaussian(
    precision, precision * Matrix.zeros(2, 1),
    Double.NaN)

  private var gameToSkillsFactorMsgs: Seq[CanonicalGaussian] = directSkillsFactors.map { gameFactor =>

    priorGameToSkillsFactorMsg
  }

  def sendMsgs() {
    //compute gameToSkillsFactorMsgs
    gameToSkillsFactorMsgs = directSkillsFactors.zip(gameToSkillsFactorMsgs).map {
      case (gameFactor, gameToSkillsMsg) => {

        skillsMarginal = updateMarginal2(skillsMarginal, gameToSkillsMsg, gameFactor.p1Index, gameFactor.p2Index, add = false)

        val (mean, variance) = (skillsMarginal.m, skillsMarginal.v)
        val directSkillsMean = Matrix(mean(gameFactor.p1Index), mean(gameFactor.p2Index))
        val var00 = variance(gameFactor.p1Index, gameFactor.p1Index)
        val var01 = variance(gameFactor.p1Index, gameFactor.p2Index)
        val var10 = variance(gameFactor.p2Index, gameFactor.p1Index)
        val var11 = variance(gameFactor.p2Index, gameFactor.p2Index)
        val directSkillsVariance = Matrix(2, 2, Array(var00, var01, var10, var11))

        val skillsToGameMsg = CanonicalGaussian(directSkillsMean, directSkillsVariance)

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

        val newDirectSkills = model.skillMarginals(skillsToGameMsg, gameFactor.game.pointsWon, gameFactor.game.pointsTotal, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        if (!directSkillsMsg.mean(0).isNaN()) {
          skillsMarginal = updateMarginal2(skillsMarginal, directSkillsMsg, gameFactor.p1Index, gameFactor.p2Index, add = true)
          directSkillsMsg
        } else {
          skillsMarginal = updateMarginal2(skillsMarginal, gameToSkillsMsg, gameFactor.p1Index, gameFactor.p2Index, add = true)
          gameToSkillsMsg
        }
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

object DeltaGPSkillsFactorGraph {

  case class GameFactor(game: Game, p1Index: Int, p2Index: Int)
  object GameFactor {
    def toGameFactors(games: Seq[Game], playerSkills: GPSkills): Seq[GameFactor] = {
      val gameFactors: Seq[GameFactor] = games.map { g =>
        val p1Index = playerSkills.skillIndexOnServe(g.playerOnServe, g.matchResult)
        val p2Index = playerSkills.skillIndexOnReturn(g.playerOnReturn, g.matchResult)

        GameFactor(g, p1Index, p2Index)
      }
      gameFactors
    }
  }

  case class Game(playerOnServe: String, playerOnReturn: String, pointsWon: Int, pointsTotal: Int, matchResult: MatchResult)
  object Game {
    def toGames(matchResults: Seq[MatchResult]): Seq[Game] = {
      val games = matchResults.flatMap { r =>

        val p1OnServeGame = Game(r.player1, r.player2, r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal, r)
        val p2OnServeGame = Game(r.player2, r.player1, r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal, r)

        List(p1OnServeGame, p2OnServeGame)

      }
      games
    }
  }

}