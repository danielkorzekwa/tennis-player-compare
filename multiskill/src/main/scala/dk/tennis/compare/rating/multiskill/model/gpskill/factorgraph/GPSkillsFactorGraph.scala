package dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear._
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import GPSkillsFactorGraph._
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.model.gpskill.GPSkills
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import scala.math._
import dk.bayes.math.gaussian.MultivariateGaussian
import com.typesafe.scalalogging.slf4j.Logging

case class GPSkillsFactorGraph(priorPlayerSkills: GPSkills,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends Logging {

  private val canonPriorSkills = CanonicalGaussian(priorPlayerSkills.skillsGaussian.m, priorPlayerSkills.skillsGaussian.v)

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
      case (gameFactor, gameToSkillsMsg) =>

        val (mean, variance) = (skillsMarginal.m, skillsMarginal.v)
        val directSkillsMean = Matrix(mean(gameFactor.p1Index), mean(gameFactor.p2Index))
        val var00 = variance(gameFactor.p1Index, gameFactor.p1Index)
        val var01 = variance(gameFactor.p1Index, gameFactor.p2Index)
        val var10 = variance(gameFactor.p2Index, gameFactor.p1Index)
        val var11 = variance(gameFactor.p2Index, gameFactor.p2Index)
        val directSkillsVariance = Matrix(2, 2, Array(var00, var01, var10, var11))

        val skillsToGameMsg = CanonicalGaussian(directSkillsMean, directSkillsVariance) / gameToSkillsMsg

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

        val newDirectSkills = model.skillMarginals(skillsToGameMsg, gameFactor.game.pointsWon, gameFactor.game.pointsTotal, threshold = 1e-4)

        val directSkillsMsg = newDirectSkills / skillsToGameMsg

        directSkillsMsg
    }

    //compute marginal

    val canonSkillsMarginal = directSkillsFactors.zip(gameToSkillsFactorMsgs).foldLeft(canonPriorSkills.copy()) {
      case (marginal, (gameFactor, gameToSkillMsg)) =>

        val k00 = marginal.k(gameFactor.p1Index, gameFactor.p1Index)
        marginal.k.set(gameFactor.p1Index, gameFactor.p1Index, k00 + gameToSkillMsg.k(0, 0))

        val k01 = marginal.k(gameFactor.p1Index, gameFactor.p2Index)
        marginal.k.set(gameFactor.p1Index, gameFactor.p2Index, k01 + gameToSkillMsg.k(0, 1))

        val k10 = marginal.k(gameFactor.p2Index, gameFactor.p1Index)
        marginal.k.set(gameFactor.p2Index, gameFactor.p1Index, k10 + gameToSkillMsg.k(1, 0))

        val k11 = marginal.k(gameFactor.p2Index, gameFactor.p2Index)
        marginal.k.set(gameFactor.p2Index, gameFactor.p2Index, k11 + gameToSkillMsg.k(1, 1))

        val h0 = marginal.h(gameFactor.p1Index)
        marginal.h.set(gameFactor.p1Index, 0, h0 + gameToSkillMsg.h(0))

        val h1 = marginal.h(gameFactor.p2Index)
        marginal.h.set(gameFactor.p2Index, 0, h1 + gameToSkillMsg.h(1))

        marginal

    }

    skillsMarginal = MultivariateGaussian(canonSkillsMarginal.mean, canonSkillsMarginal.variance)

  }

  def getPlayerSkillsMarginal(): MultivariateGaussian = skillsMarginal

}

object GPSkillsFactorGraph {

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