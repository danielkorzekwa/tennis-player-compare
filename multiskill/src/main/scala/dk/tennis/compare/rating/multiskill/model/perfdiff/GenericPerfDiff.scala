package dk.tennis.compare.rating.multiskill.model.perfdiff

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.CanonicalGaussian
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.calibrate
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph

case class GenericPerfDiff(skillsFactor: SkillsFactor, perfVarOnServe: Double, perfVarOnReturn: Double, scores: Array[Score],
  threshold: Double = 1e-4) extends Logging {

  logger.info("Creating factor graph")
  private val skillsFactorGraph = SkillsFactorGraph(scores, perfVarOnServe, perfVarOnReturn, skillsFactor)

  logger.info("Calibrating factor graph")
  calibrate(skillsFactorGraph, threshold)

  /**
   * Returns Tuple3(
   * - Perf diffs for all games,
   * - Partial derivatives for the mean of the game performance difference with respect to some hyper parameters
   * - Partial derivatives for the variance of the game performance difference with respect to some hyper parameters
   */
  def inferPerfDiffs(): Tuple3[Array[Gaussian], Matrix, Matrix] = {
    val perfDiffs = getPerfDiffToOutcomeMsgs().toArray

    val perfDiffsD = getPerfDiffToOutcomeMsgsD()
    val perfDiffsMeanD = perfDiffsD.map(v => v._1)
    val perfDiffsVarD = perfDiffsD.map(v => v._2)

    (perfDiffs, Matrix(perfDiffsMeanD), Matrix(perfDiffsVarD))
  }

  private def getPerfDiffToOutcomeMsgs(): Seq[Gaussian] = {

    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs()

    val perfDiffToOutcomeMsgs = skillsToGameMsgs.map { skillsToGameMsg =>
      val A_d = Matrix(1d, -1d).t
      val V_d = Matrix(2, 2, Array(perfVarOnServe, 0, 0, perfVarOnReturn))

      val perfDiffMsg = MultivariateGaussian((A_d * skillsToGameMsg.m), (A_d * (skillsToGameMsg.v + V_d) * A_d.t)).toGaussian

      perfDiffMsg
    }

    perfDiffToOutcomeMsgs
  }

  private def getPerfDiffToOutcomeMsgsD(): Array[Tuple2[Array[Double], Array[Double]]] = {

    val gameSkillsMarginals = skillsFactor.getGameSkillsMarginals(skillsFactorGraph.gameToSkillsFactorMsgs)
    val gameSkillsMarginalsD = skillsFactor.getGameSkillsMarginalsD(skillsFactorGraph.gameToSkillsFactorMsgs)
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs()

    val perfDiffToOutcomeMsgsD = (0 until scores.size).map { index =>

      val skillsToGameMsg = skillsToGameMsgs(index)
      val gameSkillsMarginal = gameSkillsMarginals(index)
      val gameSkillsMarginalD = gameSkillsMarginalsD(index)

      val skillsToGameMsgVarD = skillsToGameMsg.variance * gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * skillsToGameMsg.variance

      val h_d = -1 * (gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * gameSkillsMarginal.m) + gameSkillsMarginal.variance.inv * gameSkillsMarginalD.m
      val skillsToGameMsgMeanD = skillsToGameMsgVarD * skillsToGameMsg.h + skillsToGameMsg.variance * h_d
      val skillsToGameMsgD = MultivariateGaussian(skillsToGameMsgMeanD, skillsToGameMsgVarD)

      val A_d = Matrix(1d, -1d).t
      val V_d = Matrix(2, 2, Array(perfVarOnServe, 0, 0, perfVarOnReturn))

      val muD = (A_d * skillsToGameMsgD.m).at(0)
      val varD = (A_d * skillsToGameMsgD.v * A_d.t).at(0)

      (Array(muD), Array(varD))

    }.toArray

    perfDiffToOutcomeMsgsD

  }

  private implicit def MvnGaussian(canon: CanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }

}