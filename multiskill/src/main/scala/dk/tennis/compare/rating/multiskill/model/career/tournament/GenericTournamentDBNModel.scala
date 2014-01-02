package dk.tennis.compare.rating.multiskill.model.career.tournament

import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.factorgraph.factor.CachedTennisMatchFactor
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.infer.ep.calibrate.fb.EPSummary
import dk.bayes.infer.ep.GenericEP
import GenericTournamentDBNModel._
import dk.bayes.model.factor.GaussianFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

object GenericTournamentDBNModel {

  implicit def toPlayerSkill(factor: GaussianFactor): PlayerSkill = PlayerSkill(factor.m, factor.v)
}

case class GenericTournamentDBNModel(beforeTSkills: Map[String, PlayerSkills], t: TournamentResult, perfVarOnServe: Double, perfVarOnReturn: Double) extends TournamentDBNModel {

  val nextVarId = new AtomicInteger(0)
  val factorGraph = GenericFactorGraph()

  val skillFactorsOnServe: Map[String, GaussianFactor] = beforeTSkills.map {
    case (player, skills) =>
      val skillFactorOnServe = new GaussianFactor(nextVarId.getAndIncrement(), skills.skillOnServe.mean, skills.skillOnServe.variance)
      factorGraph.addFactor(skillFactorOnServe)
      (player, skillFactorOnServe)
  }

  val skillFactorsOnReturn: Map[String, GaussianFactor] = beforeTSkills.map {
    case (player, skills) =>
      val skillFactorOnReturn = new GaussianFactor(nextVarId.getAndIncrement(), skills.skillOnReturn.mean, skills.skillOnReturn.variance)
      factorGraph.addFactor(skillFactorOnReturn)
      (player, skillFactorOnReturn)
  }

  addMatchFactors()

  /**
   * Calibrates factor graph.
   *
   * @param maxIter The maximum number of iterations that EP is executed for
   * @param currIterProgress Current iteration number. It is called by the calibrate method at the beginning of every iteration
   *
   * @return EP execution summary
   */
  def calibrate(maxIter: Int, currIterProgress: (Int) => Unit): EPSummary = {
    ForwardBackwardEPCalibrate(factorGraph).calibrate(maxIter, currIterProgress)
  }

  def getAfterTSkills(): Map[String, PlayerSkills] = {

    val infer = GenericEP(factorGraph)

    val afterTSkills = beforeTSkills.map {
      case (player, skills) =>

        val afterTSkillOnServe: PlayerSkill = infer.marginal(skillFactorsOnServe(player).varId).asInstanceOf[GaussianFactor]
        val afterTSkillOnReturn: PlayerSkill = infer.marginal(skillFactorsOnReturn(player).varId).asInstanceOf[GaussianFactor]
        (player, skills.copy(skillOnServe = afterTSkillOnServe, skillOnReturn = afterTSkillOnReturn))
    }

    afterTSkills
  }

  private def addMatchFactors() {

    t.matchResults.map { r =>

      val player1FactorOnServe = skillFactorsOnServe(r.player1)
      val player1FactorOnReturn = skillFactorsOnReturn(r.player1)

      val player2FactorOnServe = skillFactorsOnServe(r.player2)
      val player2FactorOnReturn = skillFactorsOnReturn(r.player2)

      val player1VarId = nextVarId.getAndIncrement()
      val player2VarId = nextVarId.getAndIncrement()
      val outcomeVarId = nextVarId.getAndIncrement()

      val player1Factor = PlayerFactor(player1FactorOnServe.varId, player1FactorOnReturn.varId, player1VarId)
      val player2Factor = PlayerFactor(player2FactorOnServe.varId, player2FactorOnReturn.varId, player2VarId)

      val p1Points = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
      val p2Points = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
      val game = Game(t.tournamentTime, r.player1, r.player2, p1Points, p2Points)
      val matchFactor = new CachedTennisMatchFactor(player1Factor, player2Factor, outcomeVarId, perfVarOnServe, perfVarOnReturn, game)

      factorGraph.addFactor(player1Factor)
      factorGraph.addFactor(player2Factor)
      factorGraph.addFactor(matchFactor)
    }
  }
}