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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeCovFunc

case class ScoresSimulator {

  private val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)

  def skillMeanFunc(player: Player): Double = { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }

  def simulate(scores: Array[Score], opponentMap: Map[String, OpponentType], randSeed: Int): Tuple2[Array[SimScore], Double] = {

    val trueParams = DenseVector(log(0.000000000000000001), log(1), log(1), 2.3)

    val covFunc = OpponentTypeCovFunc(trueParams.data.dropRight(1), opponentMap)
    val simScores = scoreSim(scores, skillMeanFunc, covFunc, logPerfStdDev = trueParams.data.last, randSeed)

    val trueLoglik = SkillsDiffFunction(simScores.map(s => s.score), skillMeanFunc, None, (state) => {}, covFunc).calculate(trueParams)._1

    (simScores, trueLoglik)
  }

}