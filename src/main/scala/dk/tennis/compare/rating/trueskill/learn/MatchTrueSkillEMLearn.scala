package dk.tennis.compare.rating.trueskill.learn

import dk.tennis.compare.rating.trueskill.model.Result
import scala.annotation.tailrec
import dk.tennis.compare.rating.trueskill.ratingdbn.GenericTrueSkillDbn
import dk.tennis.compare.rating.trueskill.factorgraph.TennisDeepDbnFactorGraph
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.learn.lds.LatentVariable
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.GenericLDSLearn
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import dk.bayes.model.factorgraph.FactorGraph
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate

/**
 * Learns skill transition variance based on match outcomes and single skill for every player.
 *
 * @author Daniel Korzekwa
 */
object MatchTrueSkillEMLearn extends TrueSkillEMLearn {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  def learn(skillTransVariance: Double, perfVariance: Double, results: Seq[Result], maxIter: Int): Double = {

    @tailrec
    def learnRecursion(currSkillTransVariance: Double, currIter: Int): Double = {

      logger.debug("TrueSkillEM iter=%d, skillTransVariance=%.6f".format(currIter, currSkillTransVariance))

      val tennisFactorGraph = TennisDeepDbnFactorGraph(currSkillTransVariance, perfVariance)
      results.foreach(r => tennisFactorGraph.addResult(r))

      val ep = GenericEP(tennisFactorGraph.getFactorGraph())
      def progress(currIter: Int) = {} //println("EP iteration: " + currIter)
      val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())
      logger.debug("Iter total: " + epCalibrate.calibrate(1000, progress))

      val newVariance = mStep(tennisFactorGraph)

      if (currIter < maxIter) learnRecursion(newVariance, currIter + 1)
      else newVariance
    }

    val learnedVariance = learnRecursion(skillTransVariance, 0)
    learnedVariance
  }

  /**
   * Returns maximised skill transition variance.
   *
   * @param em Calibrated factor graph
   */
  private def mStep(tennisFactorGraph: TennisDeepDbnFactorGraph): Double = {
    val ep = GenericEP(tennisFactorGraph.getFactorGraph())

    val sequences = tennisFactorGraph.getSkillVarIds().values.filter(varIds => varIds.size >= 2).map { varIds =>
      val priorFactor = ep.marginal(varIds.head).asInstanceOf[GaussianFactor]
      val priorLatentVariable = LatentVariable(priorFactor.m, priorFactor.v, None)

      val transitionVariables = varIds.sliding(2).map {
        case Seq(varT0, varT1) =>
          val factorT1 = ep.marginal(varT1).asInstanceOf[GaussianFactor]
          val transitionFactor = ep.marginal(varT0, varT1).asInstanceOf[BivariateGaussianFactor]
          val transitionLatentVariable = LatentVariable(factorT1.m, factorT1.v, Some(transitionFactor.variance.at(1)))
          transitionLatentVariable
      }.toList

      priorLatentVariable :: transitionVariables

    }.toSeq
    val newVariance = GenericLDSLearn.newQ(sequences)
    newVariance
  }
}