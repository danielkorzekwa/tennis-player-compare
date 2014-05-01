package dk.tennis.compare.rating.multiskill.model.gpskill
import scala.annotation.tailrec
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph.GPSkillsFactorGraph

case class GenericGPSkillsInfer(perfVarOnServe: Double, perfVarOnReturn: Double,players:Array[Player]) extends GPSkillsInfer with Logging {

  def skillsMarginal(skillsGaussian: MultivariateGaussian, threshold: Double = 1e-4): MultivariateGaussian = {

    logger.info("Creating factor graph")
    val factorGraph = GPSkillsFactorGraph(skillsGaussian,players, perfVarOnServe, perfVarOnReturn)

    @tailrec
    def calibrate(playerSkillsMarginal: MultivariateGaussian): MultivariateGaussian = {
      logger.info("Calibrating tournament model...")
      factorGraph.sendMsgs()

      val newSkillsMarginal = factorGraph.getPlayerSkillsMarginal()

      if (equals(newSkillsMarginal, playerSkillsMarginal, threshold)) newSkillsMarginal
      else calibrate(newSkillsMarginal)
    }

    val skillsMarginal = calibrate(skillsGaussian)
    skillsMarginal

  }

  def equals(gaussian1: MultivariateGaussian, gaussian2: MultivariateGaussian, threshold: Double): Boolean = {

    val (mean1, variance1) = (gaussian1.m, gaussian1.v)
    val (mean2, variance2) = (gaussian2.m, gaussian2.v)

    mean1.matrix.isIdentical(mean2.matrix, threshold) 
     // variance1.matrix.isIdentical(variance2.matrix, threshold)

  }
}