package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.learn.lds.GenericLDSLearn
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.factorgraph.TennisDbnFactorGraph
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import scala.math._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.infer.ep.EP
import dk.bayes.learn.lds.TransitionStat
import dk.bayes.learn.lds.PriorStat
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.model.od.GenericOffenceDefenceModel
import dk.tennis.compare.rating.multiskill.model.od.utils.Predict
import dk.tennis.compare.rating.multiskill.model.od.utils.LogLik

/**
 * Learns skill transition variance based on tennis point outcomes and skills on serve and return for both players.
 *
 * @author Daniel Korzekwa
 */
object GenericMultiSkillEMLearn extends MultiSkillEMLearn {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  private def nilIterStatus(emStatus: EMStatus): Unit = {}

  def learn(multiSkillParams: MultiSkillParams, tournaments: Seq[TournamentResult], maxIter: Int, iterStatus: EMStatus => Unit = nilIterStatus): MultiSkillParams = {
    require(maxIter > 0, "Number of EM iterations is zero")

    @tailrec
    def learnRecursion(currMultiSkillParams: MultiSkillParams, currIter: Int): MultiSkillParams = {

      val loglikMatch = calcLoglikMatch(currMultiSkillParams, tournaments)

      iterStatus(EMStatus(currIter, currMultiSkillParams, loglikMatch))

      val tennisFactorGraph = TennisDbnFactorGraph(currMultiSkillParams)
      tournaments.foreach(t => tennisFactorGraph.addTournament(t))

      //E-step
      val epCalibrate = ForwardBackwardEPCalibrate(tennisFactorGraph.getFactorGraph())

      val calibrateIterNum = epCalibrate.calibrate(1000, iterNum => {})
      logger.debug("EP calibration iterations: " + calibrateIterNum.iterNum)

      //M-step
      val newParams = Mstep.maximise(tennisFactorGraph, currMultiSkillParams)

      if (currIter < maxIter) learnRecursion(newParams, currIter + 1)
      else newParams
    }

    val learnedParams = if (tournaments.isEmpty) multiSkillParams else learnRecursion(multiSkillParams, 1)
    learnedParams

  }

  /**Returns [total,avg] log likelihood*/
  private def calcLoglikMatch(multiSkillParams: MultiSkillParams, tournaments: Seq[TournamentResult]): Tuple2[Double, Double] = {

    val games = tournaments.flatMap { t =>
      t.matchResults.map { r =>

        val p1Points = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
        val p2Points = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
        val game = Game(t.tournamentTime, r.player1, r.player2, p1Points, p2Points)
        game

      }
    }
    val model = GenericOffenceDefenceModel(multiSkillParams)
    val predictions = Predict.predict(model, games)
    val (totalLogLik, avgLogLik) = LogLik.logLik(predictions.map(p => Tuple3(p.pointProb,p.points._1,p.points._2)))

    (totalLogLik, avgLogLik)

  }

}