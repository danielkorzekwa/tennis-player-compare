package dk.tennis.compare.rating.multiskill.model.perfdiff

import scala.math._
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.SkillsFactorGraph
import dk.tennis.compare.rating.multiskill.model.perfdiff.factorgraph.calibrate
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.SkillsFactor
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.gameSkillsToPerfDiffs
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.multigp.PlayerSkills
import dk.tennis.compare.rating.multiskill.infer.skillgivenskills.inferSkillGivenSkills

case class GenericPerfDiffModel(createPlayersSkillsFactor: (Array[Player]) => SkillsFactor, logPerfStdDev: Double, scores: Array[Score],
  threshold: Double = 1e-3) extends PerfDiffModel with Logging {

  logger.debug("Creating factor graph")
  val players = Score.toPlayers(scores)
  val skillsFactor = createPlayersSkillsFactor(players)
  val skillsFactorGraph = SkillsFactorGraph(scores, logPerfStdDev, skillsFactor)

  def calibrateModel() {
    logger.debug("Calibrating factor graph")
    calibrate(skillsFactorGraph, threshold)
    logger.debug("Calibrating factor graph - completed")
  }

  def inferPerfDiffs(): Array[PerfDiff] = {
    val gameSkillsMarginals = skillsFactorGraph.allSkills.getGameSkillsMarginals()
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals).map(toMvnGaussian(_))

    val perfDiffs = gameSkillsToPerfDiffs(skillsToGameMsgs, logPerfStdDev).toArray

    perfDiffs

  }

  def calcPlayerSkill(player: Player): Gaussian = {

    skillsFactorGraph.allSkills.calcPlayerSkill(player)
  }

  def calcPosteriorSkillsForPlayer(playerName: String, skillOnServe: Boolean): PlayerSkills = {
    skillsFactorGraph.allSkills.calcPosteriorSkillsForPlayer(playerName, skillOnServe)
  }

  def inferPerfDiffsWithD(): Tuple3[Array[PerfDiff], Matrix, Matrix] = {

    val (gameSkillsMarginals, gameSkillsMarginalsD) = skillsFactorGraph.allSkills.getGameSkillsMarginalsWithD()
    val skillsToGameMsgs = skillsFactorGraph.calcSkillsToGameMsgs(gameSkillsMarginals)

    val perfDiffs = gameSkillsToPerfDiffs(skillsToGameMsgs.map(toMvnGaussian(_)), logPerfStdDev).toArray

    val (perfDiffsMeanD, perfDiffsVarD) = getPerfDiffToOutcomeMsgsD(skillsToGameMsgs, gameSkillsMarginals, gameSkillsMarginalsD)
    (perfDiffs, perfDiffsMeanD, perfDiffsVarD)
  }

  private def getPerfDiffToOutcomeMsgsD(skillsToGameMsgs: Seq[CanonicalGaussian], gameSkillsMarginals: Seq[CanonicalGaussian],
    gamesSkillsMarginalsD: Seq[Seq[MultivariateGaussian]]): Tuple2[Matrix, Matrix] = {

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

      val gameSkillsMarginalsDs = gamesSkillsMarginalsD(index)

      //*Seq of tuple(meanD,VarD) for n hyper parameters
      val perfDiffDs: Seq[Tuple2[Double, Double]] = gameSkillsMarginalsDs.map(gameSkillsMarginalsD => perfDiffD(gameSkillsMarginalsD))
      val perfDiffDMean: Array[Double] = perfDiffDs.map(d => d._1).toArray
      val perfDiffDVar: Array[Double] = perfDiffDs.map(d => d._2).toArray

      val A = Matrix(1d, -1d).t
      val muD_perfVar = 0d
      val varD_perfVar = (A * Matrix(2, 2, Array(2 * exp(2 * logPerfStdDev), 0, 0, 2 * exp(2 * logPerfStdDev))) * A.t).at(0)
      (perfDiffDMean :+ muD_perfVar, perfDiffDVar :+ varD_perfVar)

    }.toArray

    val hypSize = perfDiffToOutcomeMsgsD.head._1.size
    val perfDiffToOutcomeMsgsMeanD = Matrix(scores.size, hypSize, perfDiffToOutcomeMsgsD.flatMap(_._1))
    val perfDiffToOutcomeMsgsVarD = Matrix(scores.size, hypSize, perfDiffToOutcomeMsgsD.flatMap(_._2))
    (perfDiffToOutcomeMsgsMeanD, perfDiffToOutcomeMsgsVarD)

  }

  private implicit def toMvnGaussian(canon: CanonicalGaussian): MultivariateGaussian = {
    MultivariateGaussian(canon.mean, canon.variance)
  }

}