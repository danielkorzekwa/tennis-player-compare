package dk.tennis.compare.rating.multiskill.scoresim

import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import breeze.linalg.DenseVector
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentTypeOverTimeCovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.learn.PlayerCovFuncFactory
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponenttype.OpponentType
import dk.tennis.compare.rating.multiskill.learn.SkillsDiffFunction
import ScoresSimulator._
import dk.tennis.compare.rating.multiskill.infer.skillsgivenopponent.SkillsGivenOpponent

case class ScoresSimulator {

  private val (trueSkillMeanOnServe, trueSkillMeanOnReturn) = (5d, 0)
 
  def skillMeanFunc(player: Player): Double = { if (player.onServe) trueSkillMeanOnServe else trueSkillMeanOnReturn }
  
  def simulate(scores: Array[Score], opponentMap: Map[String, OpponentType],randSeed: Int): Tuple2[Array[SimScore], Double] = {

    val trueParams = DenseVector(log(0.0000000000001), log(1), log(1),
       -0.625343662255204, 3.263911687513335, -0.04617824159058743, 6.556709591597913, 2.3)

    val trueSkillCovFactory = TrueSkillCovFactory(opponentMap)

    val covFunc = trueSkillCovFactory.create(trueParams.data.dropRight(1), Map(), Map())
    val simScores = scoreSim(scores, skillMeanFunc, covFunc, logPerfStdDev = trueParams.data.last,randSeed)

    val priorSkillsOnServeGivenOpponent = Map[String, Seq[PlayerSkill]]()
    val priorSkillsOnReturnGivenOpponent = Map[String, Seq[PlayerSkill]]()
    
    val trueLoglik = SkillsDiffFunction(simScores.map(s => s.score), skillMeanFunc,  None,(state) => {},covFunc).calculate(trueParams)._1

    (simScores, trueLoglik)
  }

 
}

object ScoresSimulator {
   case class TrueSkillCovFactory(opponentMap: Map[String, OpponentType]) extends PlayerCovFuncFactory {

    def create(params: Seq[Double], skillsOnServeGivenOpponent: Map[String, Seq[PlayerSkill]], skillsOnReturnGivenOpponent: Map[String, Seq[PlayerSkill]]): CovFunc = {
      OpponentTypeOverTimeCovFunc(params, opponentMap)
    }
  }
}