package dk.tennis.compare.rating.multiskill.model.career.tournament

import java.util.concurrent.atomic.AtomicInteger

import dk.bayes.infer.ep.GenericEP
import dk.bayes.infer.ep.calibrate.fb.EPSummary
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.gaussian.Linear.Matrix
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult

/**
 * @param allPlayers
 * @param tournament
 * @param skillsGaussian Skills on serve and return
 *
 */
case class GenericTournamentCorDBNModel(allPlayers: IndexedSeq[String], skillsGaussian: CanonicalGaussian,
  tournament: TournamentResult, perfVarOnServe: Double, perfVarOnReturn: Double) extends TournamentCorDBNModel {

  private val nextVarId = new AtomicInteger(0)
  private val factorGraph = GenericFactorGraph()

  private val skillsFactor = MvnGaussianFactor(nextVarId.getAndIncrement(), skillsGaussian)

  factorGraph.addFactor(skillsFactor)

  //add match factors
  tournament.matchResults.foreach { result =>
    val matchFactorP1OnServe = CachedTennisMatchCorFactor(
      result.player1, result.player2, result.p1Stats.servicePointsWon, result.p1Stats.servicePointsTotal,
      allPlayers, skillsFactor, nextVarId.getAndIncrement(),
      perfVarOnServe, perfVarOnReturn)

    val matchFactorP2OnServe = CachedTennisMatchCorFactor(
      result.player2, result.player1, result.p2Stats.servicePointsWon, result.p2Stats.servicePointsTotal,
      allPlayers, skillsFactor, nextVarId.getAndIncrement(),
      perfVarOnServe, perfVarOnReturn)

    factorGraph.addFactor(matchFactorP1OnServe)
    factorGraph.addFactor(matchFactorP2OnServe)
  }

  /**
   * Calibrates factor graph.
   *
   * @param maxIter The maximum number of iterations that EP is executed for
   * @param currIterProgress Current iteration number. It is called by the calibrate method at the beginning of every iteration
   *
   * @return EP execution summary
   */
  def calibrate(maxIter: Int, currIterProgress: (Int) => Unit): EPSummary = {
    ForwardBackwardEPCalibrate(factorGraph, threshold = 0.005).calibrate(maxIter, currIterProgress)
  }

  def getAfterTSkills: CanonicalGaussian = {
    val infer = GenericEP(factorGraph)
    infer.marginal(skillsFactor.varId).asInstanceOf[MvnGaussianFactor].canonGaussian
  }

}