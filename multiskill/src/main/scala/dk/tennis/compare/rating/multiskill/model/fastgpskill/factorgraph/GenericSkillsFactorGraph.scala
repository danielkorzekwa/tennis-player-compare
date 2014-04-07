package dk.tennis.compare.rating.multiskill.model.fastgpskill.factorgraph

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.fastgpskill.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel

case class GenericSkillsFactorGraph(priorSupportSkills: MultivariateGaussian, allVsSupportCov: Matrix, players: Array[Player],
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends SkillsFactorGraph {

  private val priorGameToSkillsMsg = MultivariateGaussian(
    Matrix.zeros(2, 1), Matrix(2, 2, Array.fill(4)(Double.PositiveInfinity)))

  private var gameToSkillsMsgs: Seq[MultivariateGaussian] = (1 to players.size / 2).map { i =>
    priorGameToSkillsMsg
  }

  private var supportSkillsMarginal = priorSupportSkills

  def sendMsgs() {

    val Af = allVsSupportCov * supportSkillsMarginal.v.inv
    val fMean = Af * supportSkillsMarginal.m

    //compute new game to skill msgs
    gameToSkillsMsgs.zipWithIndex.map {
      case (gameToSkillMsg, index) =>

        val AgAf = Af.extractRow(index * 2).combine(1, 0, Af.extractRow(index * 2 + 1))

        val skillsToGameMsgMean = fMean.extractRow(index * 2).combine(1, 0, fMean.extractRow(index * 2 + 1))
        val skillsToGameMsgVar = (AgAf * supportSkillsMarginal.v * AgAf.t)
        val skillsToGameMsg = CanonicalGaussian(skillsToGameMsgMean, skillsToGameMsgVar)

        println(skillsToGameMsgVar)
        
        val gameToOutcomeMsg = skillsToGameMsg / CanonicalGaussian(gameToSkillMsg.m, gameToSkillMsg.v)

        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)

        val p1PointsWon = players(index * 2).pointsWon
        val p2PointsWon = players(index * 2 + 1).pointsWon

        val newDirectSkills = model.skillMarginals(gameToOutcomeMsg, p1PointsWon, p1PointsWon + p2PointsWon, threshold = 1e-4)

        val newGameToSkillMsg = newDirectSkills / gameToOutcomeMsg
        println("a")
    }
    println("end")

    //compute new skills marginal
  }

  def getSupportSkillsMarginal(): MultivariateGaussian = supportSkillsMarginal
}