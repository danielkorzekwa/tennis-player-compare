package dk.tennis.compare.rating.multiskill.model.perfdiff

import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.CanonicalGaussian
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.calibrate
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph

case class GenericPerfDiff(skillsFactorGraph: SkillsFactorGraph, logPerfStdDev: Double, scores: Array[Score],
  threshold: Double = 1e-4) extends Logging {

  logger.info("Creating factor graph")

  logger.info("Calibrating factor graph")
  calibrate(skillsFactorGraph, threshold)
  logger.info("Calibrating factor graph - completed")

  def inferPerfDiffs(): Array[Gaussian] = {
    val gameSkillsMarginals = skillsFactorGraph.skillsFactor.getGameSkillsMarginals(skillsFactorGraph.gameToSkillsFactorMsgs)
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals)

    val perfDiffs = getPerfDiffToOutcomeMsgs(skillsToGameMsgs).toArray

    perfDiffs

  }

  def inferPerfDiffsWithD(): Tuple3[Array[Gaussian], Matrix, Matrix] = {

    val (gameSkillsMarginals, gameSkillsMarginalsD) = skillsFactorGraph.skillsFactor.getGameSkillsMarginalsWithD(skillsFactorGraph.gameToSkillsFactorMsgs)
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals)

    val perfDiffs = getPerfDiffToOutcomeMsgs(skillsToGameMsgs).toArray

    val (perfDiffsMeanD, perfDiffsVarD) = getPerfDiffToOutcomeMsgsD(skillsToGameMsgs, gameSkillsMarginals, gameSkillsMarginalsD)
    (perfDiffs, perfDiffsMeanD, perfDiffsVarD)
  }

  private def getPerfDiffToOutcomeMsgs(skillsToGameMsgs: Seq[CanonicalGaussian]): Seq[Gaussian] = {

    val perfDiffToOutcomeMsgs = skillsToGameMsgs.map { skillsToGameMsg =>
      val A_d = Matrix(1d, -1d).t
      val V_d = Matrix(2, 2, Array(exp(logPerfStdDev) * exp(logPerfStdDev), 0, 0, exp(logPerfStdDev) * exp(logPerfStdDev)))

      val perfDiffMsg = MultivariateGaussian((A_d * skillsToGameMsg.m), (A_d * (skillsToGameMsg.v + V_d) * A_d.t)).toGaussian

      perfDiffMsg
    }

    perfDiffToOutcomeMsgs
  }

  private def getPerfDiffToOutcomeMsgsD(skillsToGameMsgs: Seq[CanonicalGaussian], gameSkillsMarginals: Seq[CanonicalGaussian],
    gameSkillsMarginalsD: Seq[Seq[MultivariateGaussian]]): Tuple2[Matrix, Matrix] = {

    val perfDiffToOutcomeMsgsD = (0 until scores.size).map { index =>

      val skillsToGameMsg = skillsToGameMsgs(index)
      val gameSkillsMarginal = gameSkillsMarginals(index)

      def perfDiffD(gameSkillsMarginalD: MultivariateGaussian): Tuple2[Double, Double] = {
        val skillsToGameMsgVarD = skillsToGameMsg.variance * gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * skillsToGameMsg.variance

        val h_d = -1 * (gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * gameSkillsMarginal.m) + gameSkillsMarginal.variance.inv * gameSkillsMarginalD.m
        val skillsToGameMsgMeanD = skillsToGameMsgVarD * skillsToGameMsg.h + skillsToGameMsg.variance * h_d
        val skillsToGameMsgD = MultivariateGaussian(skillsToGameMsgMeanD, skillsToGameMsgVarD)

        val A = Matrix(1d, -1d).t

        val muD = (A * skillsToGameMsgD.m).at(0)
        val varD = (A * skillsToGameMsgD.v * A.t).at(0)

        (muD, varD)
      }
      //      
      //      val skillsToGameMsgVarD = skillsToGameMsg.variance * gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * skillsToGameMsg.variance
      //
      //      val h_d = -1 * (gameSkillsMarginal.variance.inv * gameSkillsMarginalD.v * gameSkillsMarginal.variance.inv * gameSkillsMarginal.m) + gameSkillsMarginal.variance.inv * gameSkillsMarginalD.m
      //      val skillsToGameMsgMeanD = skillsToGameMsgVarD * skillsToGameMsg.h + skillsToGameMsg.variance * h_d
      //      val skillsToGameMsgD = MultivariateGaussian(skillsToGameMsgMeanD, skillsToGameMsgVarD)
      //
      //      val A = Matrix(1d, -1d).t
      //
      //      val muD = (A * skillsToGameMsgD.m).at(0)
      //      val varD = (A * skillsToGameMsgD.v * A.t).at(0)

      val gameSkillsMarginalD_sf = gameSkillsMarginalsD(index)(0)
      val (muD_sf, varD_sf) = perfDiffD(gameSkillsMarginalD_sf)

      val gameSkillsMarginalD_ell = gameSkillsMarginalsD(index)(1)
      val (muD_ell, varD_ell) = perfDiffD(gameSkillsMarginalD_ell)

      //derivatives with respect to performance variance
      val A = Matrix(1d, -1d).t
      val muD_perfVar = 0
      val varD_perfVar = (A * Matrix(2, 2, Array(2 * exp(2 * logPerfStdDev), 0, 0, 2 * exp(2 * logPerfStdDev))) * A.t).at(0)

      (Array(muD_sf, muD_ell, muD_perfVar), Array(varD_sf, varD_ell, varD_perfVar))

    }.toArray

    val perfDiffToOutcomeMsgsMeanD = Matrix(scores.size, 3, perfDiffToOutcomeMsgsD.flatMap(_._1))
    val perfDiffToOutcomeMsgsVarD = Matrix(scores.size, 3, perfDiffToOutcomeMsgsD.flatMap(_._2))
    (perfDiffToOutcomeMsgsMeanD, perfDiffToOutcomeMsgsVarD)

  }

  private implicit def MvnGaussian(canon: CanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }

}