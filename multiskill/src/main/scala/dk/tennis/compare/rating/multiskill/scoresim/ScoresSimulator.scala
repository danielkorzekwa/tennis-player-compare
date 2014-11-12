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
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc

case class ScoresSimulator {

  private val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)

  def skillMeanFunc(player: Player): Double = { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }

  def simulate(scores: Array[Score], opponentMap: Map[String, OpponentType], randSeed: Int): Tuple2[Array[SimScore], Double] = {

    val trueCovParams = Array(
        log(100), //opponent
        log(200),log(50),log(200), //surface
        -1.0394676060535801, 3.8382339487840085, 0.0032389722419957287, 8.282433925904247 //time
        )
    val logPerfStdDev = 2.3

    val covFunc = SkillCovFunc(trueCovParams)
    val simScores = scoreSim(scores, skillMeanFunc, covFunc, logPerfStdDev, randSeed)

    val trueLoglik = SkillsDiffFunction(simScores.map(s => s.score), skillMeanFunc, None, (state) => {}, covFunc).calculate(DenseVector(trueCovParams :+ logPerfStdDev))._1

    (simScores, trueLoglik)
  }

}