package dk.tennis.compare.rating.multiskill.model.perfdiff

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics._
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.gaussian.canonical.DenseCanonicalGaussian
import dk.bayes.math.linear.invchol
import dk.tennis.compare.rating.multiskill.infer.perfdiffgivenskills.inferPerfDiffsGivenSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.calibrate
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc

case class GenericPerfDiffModel(meanFunc: Player => Double, playerCovFunc: CovFunc, logPerfStdDev: Double, scores: Array[Score],
                                threshold: Double = 1e-3) extends PerfDiffModel with LazyLogging {

  logger.debug("Creating factor graph")
  val skillsFactorGraph = SkillsFactorGraph(meanFunc, playerCovFunc, scores, logPerfStdDev)

  def calibrateModel() = {
    val now = System.currentTimeMillis()
    logger.info("Calibrating factor graph")
    calibrate(skillsFactorGraph, threshold)
    logger.info("Calibrating factor graph - completed: " + (System.currentTimeMillis()-now))
  }

  def inferPerfDiffs(): Array[PerfDiff] = {
    val gameSkillsMarginals = skillsFactorGraph.allSkills.getGameSkillsMarginals()
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals).map(toMvnGaussian(_))

    val perfDiffs = inferPerfDiffsGivenSkills(skillsToGameMsgs, logPerfStdDev).toArray

    perfDiffs

  }

  def calcPosteriorSkillsForPlayer(playerName: String, skillOnServe: Boolean): Option[PlayerSkills] = {
    skillsFactorGraph.allSkills.getPosteriorSkillsForPlayer(playerName, skillOnServe)
  }

  def inferPerfDiffsWithD(): Tuple3[Array[PerfDiff], DenseMatrix[Double], DenseMatrix[Double]] = {

    val (gameSkillsMarginals, gameSkillsMarginalsD) = skillsFactorGraph.allSkills.getGameSkillsMarginalsWithD()
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals)

    val perfDiffs = inferPerfDiffsGivenSkills(skillsToGameMsgs.map(toMvnGaussian(_)), logPerfStdDev).toArray

    val (perfDiffsMeanD, perfDiffsVarD) = getPerfDiffToOutcomeMsgsD(skillsToGameMsgs, gameSkillsMarginals, gameSkillsMarginalsD)
    (perfDiffs, perfDiffsMeanD, perfDiffsVarD)
  }

  private def getPerfDiffToOutcomeMsgsD(skillsToGameMsgs: Seq[DenseCanonicalGaussian], gameSkillsMarginals: Seq[DenseCanonicalGaussian],
                                        gamesSkillsMarginalsD: Seq[Seq[MultivariateGaussian]]): Tuple2[DenseMatrix[Double], DenseMatrix[Double]] = {

    val perfDiffToOutcomeMsgsD = (0 until scores.size).map { index =>

      val skillsToGameMsg = skillsToGameMsgs(index)
      val gameSkillsMarginal = gameSkillsMarginals(index)
      def perfDiffD(gameSkillsMarginalD: MultivariateGaussian): Tuple2[Double, Double] = {
        val skillsToGameMsgVarD = skillsToGameMsg.variance * invchol(cholesky(gameSkillsMarginal.variance).t) * gameSkillsMarginalD.v * invchol(cholesky(gameSkillsMarginal.variance).t) * skillsToGameMsg.variance

        val h_d = -1d * (invchol(cholesky(gameSkillsMarginal.variance).t) * gameSkillsMarginalD.v * invchol(cholesky(gameSkillsMarginal.variance).t) * gameSkillsMarginal.m) + invchol(cholesky(gameSkillsMarginal.variance).t) * gameSkillsMarginalD.m
        val skillsToGameMsgMeanD = skillsToGameMsgVarD * skillsToGameMsg.h + skillsToGameMsg.variance * h_d
        val skillsToGameMsgD = MultivariateGaussian(skillsToGameMsgMeanD, skillsToGameMsgVarD)

        val A = DenseMatrix(1d, -1d).t

        val muD = (A * skillsToGameMsgD.m)
        val varD = (A * skillsToGameMsgD.v * A.t)

        (muD(0), varD(0, 0))
      }

      val gameSkillsMarginalsDs = gamesSkillsMarginalsD(index)

      //*Seq of tuple(meanD,VarD) for n hyper parameters
      val perfDiffDs: Seq[Tuple2[Double, Double]] = gameSkillsMarginalsDs.map(gameSkillsMarginalsD => perfDiffD(gameSkillsMarginalsD))
      val perfDiffDMean: Array[Double] = perfDiffDs.map(d => d._1).toArray
      val perfDiffDVar: Array[Double] = perfDiffDs.map(d => d._2).toArray

      val A = DenseMatrix(1d, -1d).t
      val muD_perfVar = 0d

      val varD_perfVar = (A * new DenseMatrix(2, 2, Array(2 * exp(2 * logPerfStdDev), 0, 0, 2 * exp(2 * logPerfStdDev))).t * A.t)
      (perfDiffDMean :+ muD_perfVar, perfDiffDVar :+ varD_perfVar(0, 0))

    }.toArray

    val hypSize = perfDiffToOutcomeMsgsD.head._1.size
    val perfDiffToOutcomeMsgsMeanD = new DenseMatrix(hypSize, scores.size, perfDiffToOutcomeMsgsD.flatMap(_._1)).t
    val perfDiffToOutcomeMsgsVarD = new DenseMatrix(hypSize, scores.size, perfDiffToOutcomeMsgsD.flatMap(_._2)).t
    (perfDiffToOutcomeMsgsMeanD, perfDiffToOutcomeMsgsVarD)

  }

  private implicit def toMvnGaussian(canon: DenseCanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }

}