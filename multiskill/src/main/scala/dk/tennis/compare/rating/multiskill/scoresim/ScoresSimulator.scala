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
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams

case class ScoresSimulator {

  def simulate(scores: Array[Score], opponentMap: Map[String, OpponentType], randSeed: Int,skillsModelParams:SkillsModelParams,logPerfStdDev:Double): Tuple2[Array[SimScore], Double] = {

    val skillMeanFunc = skillsModelParams.skillMeanFunc
    val skillCovFunc = skillsModelParams.skillCovFunc
    
    val simScores = scoreSim(scores,skillMeanFunc, skillCovFunc,logPerfStdDev, randSeed)

    val trueLoglik = SkillsDiffFunction(simScores.map(s => s.score), skillMeanFunc, None, (state) => {}, skillCovFunc).calculate(DenseVector(skillCovFunc.getParams.toArray :+ logPerfStdDev))._1

    (simScores, trueLoglik)
  }

}