package dk.tennis.compare.rating.multiskill.model.fastgpskill.factorgraph

import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel
import dk.tennis.compare.rating.multiskill.model.multipointcor.GenericMultiPointCorModel

case class GenericSkillsFactorGraph(priorSupportSkills: MultivariateGaussian, allVsSupportCov: Matrix, players: Array[Player],
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends SkillsFactorGraph {

  val precision = Matrix(2, 2, Array.fill(4)(Double.PositiveInfinity))
  private val priorGameVarUpMsg = CanonicalGaussian(
    precision, Matrix.zeros(2, 1),
    Double.NaN)

  private var gameVarUpMsgs: Seq[CanonicalGaussian] = (1 to players.size / 2).map { i =>
    priorGameVarUpMsg
  }

  private var supportSkillsMarginal = priorSupportSkills

  def sendMsgs() {

    val Af = allVsSupportCov * priorSupportSkills.v.inv
    val fMean = Af * supportSkillsMarginal.m

    //compute new game to skill msgs and update (h,k) of skillsFactorUpMsg
    var skillsFactorUpMsgH = Matrix.zeros(priorSupportSkills.m.numRows, 1)
    var skillsFactorUpMsgK = Matrix.zeros(priorSupportSkills.v.numRows, priorSupportSkills.v.numRows)

    val newGameVarUpMsgs = gameVarUpMsgs.zipWithIndex.map {
      case (gameVarUpMsg, index) =>

        //compute gameFactorDownMsg
        val AgAf = Af.extractRow(index * 2).combine(1, 0, Af.extractRow(index * 2 + 1))
        val AgAf_t = AgAf.t

        val gameFactorDownMsgMean = fMean.extractRow(index * 2).combine(1, 0, fMean.extractRow(index * 2 + 1))
        val gameFactorDownMsgVar = (AgAf * supportSkillsMarginal.v * AgAf_t)
        val gameFactorDownMsg = CanonicalGaussian(gameFactorDownMsgMean, gameFactorDownMsgVar) / gameVarUpMsg

        //compute gameVarUpMsg
        val model = GenericMultiPointCorModel(perfVarianceOnServe, perfVarianceOnReturn)
        val p1PointsWon = players(index * 2).pointsWon
        val p2PointsWon = players(index * 2 + 1).pointsWon

        val newDirectSkills = model.skillMarginals(gameFactorDownMsg, p1PointsWon, p1PointsWon + p2PointsWon, maxIter = 200, threshold = 1e-4)
        val newGameVarUpMsg = newDirectSkills / gameFactorDownMsg

        // update (h,k) of skillsFactorUpMsg
        skillsFactorUpMsgH = skillsFactorUpMsgH + AgAf_t * newGameVarUpMsg.h
        skillsFactorUpMsgK = skillsFactorUpMsgK + AgAf_t * newGameVarUpMsg.k * AgAf

        newGameVarUpMsg
    }
    gameVarUpMsgs = newGameVarUpMsgs

    //compute new support skills marginal
    val supportSkillsMarginalCanon = new CanonicalGaussian(skillsFactorUpMsgK, skillsFactorUpMsgH, 1) * CanonicalGaussian(priorSupportSkills.m, priorSupportSkills.v)
    supportSkillsMarginal = MultivariateGaussian(supportSkillsMarginalCanon.mean, supportSkillsMarginalCanon.variance)

  }

  def getSupportSkillsMarginal(): MultivariateGaussian = supportSkillsMarginal
}