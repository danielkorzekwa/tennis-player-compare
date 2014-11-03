package dk.tennis.compare.rating.multiskill.scoresim

import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import breeze.linalg.DenseVector
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.PlayerCovFuncFactory
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import ScoresSimulator._
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsDiffFunction

case class ScoresSimulator {

  private val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)

  def skillMeanFunc(player: Player): Double = { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }

  def simulate(scores: Array[Score], opponentMap: Map[String, OpponentType], randSeed: Int): Tuple2[Array[SimScore], Double] = {

    val trueParams = DenseVector(log(0.0000000000001), log(1), log(1),
      -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3)

    val trueSkillCovFactory = TrueSkillCovFactory(opponentMap)

    def getPlayerSkill(player: Player): PlayerSkill = throw new UnsupportedOperationException("Not implemented yet")
    val covFunc = trueSkillCovFactory.create(trueParams.data.dropRight(1), getPlayerSkill)
    val simScores = scoreSim(scores, skillMeanFunc, covFunc, logPerfStdDev = trueParams.data.last, randSeed)

    val priorSkillsOnServeGivenOpponent = Map[String, Seq[PlayerSkill]]()
    val priorSkillsOnReturnGivenOpponent = Map[String, Seq[PlayerSkill]]()

    val trueLoglik = SkillsDiffFunction(simScores.map(s => s.score), skillMeanFunc, None, (state) => {}, covFunc).calculate(trueParams)._1

    (simScores, trueLoglik)
  }

}

object ScoresSimulator {
  case class TrueSkillCovFactory(opponentMap: Map[String, OpponentType]) extends PlayerCovFuncFactory {

    def create(params: Seq[Double], getPlayerSkill: (Player) => PlayerSkill): CovFunc = {
      OpponentTypeOverTimeCovFunc(params, opponentMap)
    }
  }
}