package dk.tennis.compare.rating.multiskill.factorgraph

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.infer.ep.calibrate.fb.ForwardBackwardEPCalibrate
import dk.bayes.model.factorgraph.GenericFactorGraph
import dk.bayes.infer.ep.calibrate.fb.EPSummary
import dk.bayes.infer.ep.calibrate.fb.EPSummary
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.bayes.model.factor.MvnGaussianFactor
import java.util.concurrent.atomic.AtomicInteger
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.bayes.math.gaussian.Linear.Matrix
import dk.bayes.model.factor.MvnGaussianFactor
import scala.collection._
import org.ejml.ops.CommonOps
import dk.bayes.math.gaussian.Linear
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.LinearGaussianFactor
import dk.bayes.model.factor.MvnLinearGaussianFactor
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.factorgraph.factor.PlayerFactor
import dk.tennis.compare.rating.multiskill.factorgraph.factor.CachedTennisMatchFactor
import dk.bayes.infer.ep.GenericEP
import dk.bayes.model.factor.MvnGaussianFactor
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.math._
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import java.util.Date

case class GPTennisFactorGraph(tournaments: Seq[TournamentResult], val perfVariance: Double, skillOnServeMean: Double, skillOnReturnMean: Double) {

  private val factorGraph = GenericFactorGraph()
  //key - player name
  private val playerGPSkillsOnServeMap: mutable.Map[String, MvnGaussianFactor] = mutable.Map()
  private val playerGPSkillsOnReturnMap: mutable.Map[String, MvnGaussianFactor] = mutable.Map()

  private val nextVarId = new AtomicInteger(1)

  private val allMatches = tournaments.flatMap(t => t.matchResults)
  val allPlayers = tournaments.flatMap(t => t.players).distinct

  build()

  private def build() {

    val tournamentSize = tournaments.size
    val priorSkillOnServeMean = Matrix(Array.fill(tournamentSize)(skillOnServeMean))
    val priorSkillOnReturn = Matrix(Array.fill(tournamentSize)(skillOnReturnMean))

    val priorSkillVariance = Matrix(tournamentSize, tournamentSize, (row, col) => kernel(tournaments(row), tournaments(col), tournaments, row, col))
    println(priorSkillVariance)
    //add skill gp factors
    allPlayers.foreach { player =>
      val skillGPFactorOnServe = MvnGaussianFactor(nextVarId.getAndIncrement(), CanonicalGaussian(priorSkillOnServeMean, priorSkillVariance))
      factorGraph.addFactor(skillGPFactorOnServe)
      playerGPSkillsOnServeMap.put(player, skillGPFactorOnServe)

      val skillGPFactorOnReturn = MvnGaussianFactor(nextVarId.getAndIncrement(), CanonicalGaussian(priorSkillOnReturn, priorSkillVariance))
      factorGraph.addFactor(skillGPFactorOnReturn)
      playerGPSkillsOnReturnMap.put(player, skillGPFactorOnReturn)
    }

    tournaments.zipWithIndex.foreach { case (t, i) => if (t.matchResults.size > 0) addTournamentFactors(t, i, tournamentSize) }

  }

  private def kernel(t1: TournamentResult, t2: TournamentResult, tournaments: Seq[TournamentResult], t1Index: Int, t2Index: Int): Double = {

    val theSameTournamentKernel = if (t1.tournamentName.equals(t2.tournamentName) && t1.tournamentTime.getTime() == t2.tournamentTime.getTime()) 0.001 else 0
    val timeDiff = abs((t1.tournamentTime.getTime() - t2.tournamentTime.getTime()) / (1000 * 3600 * 24))
    val l = 1000
    val timeDiffKernel = 1 * exp(-pow(timeDiff, 2) / (2 * l * l))
    theSameTournamentKernel + timeDiffKernel

  }

  private def addTournamentFactors(tournament: TournamentResult, index: Int, tournamentSize: Int) {
    val playerSkillsOnServeMap: mutable.Map[String, MvnLinearGaussianFactor] = mutable.Map()
    val playerSkillsOnReturnMap: mutable.Map[String, MvnLinearGaussianFactor] = mutable.Map()

    tournament.players.foreach { player =>
      val playerGPSkillOnServe = playerGPSkillsOnServeMap(player)
      val playerGPSkillOnReturn = playerGPSkillsOnReturnMap(player)
      val A = Matrix(tournamentSize, 1, Array.fill(tournamentSize)(0d))
      A.set(index, 0, 1)

      val playerSkillOnServe = MvnLinearGaussianFactor(playerGPSkillOnServe.varId, nextVarId.getAndIncrement(), A, b = 0, v = 1e-10)
      factorGraph.addFactor(playerSkillOnServe)
      playerSkillsOnServeMap.put(player, playerSkillOnServe)

      val playerSkillOnReturn = MvnLinearGaussianFactor(playerGPSkillOnReturn.varId, nextVarId.getAndIncrement(), A, b = 0, v = 1e-10)
      factorGraph.addFactor(playerSkillOnReturn)
      playerSkillsOnReturnMap.put(player, playerSkillOnReturn)
    }

    tournament.matchResults.foreach { r =>

      val p1Points = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
      val p2Points = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
      val game = Game(tournament.tournamentTime, r.player1, r.player2, p1Points, p2Points)

      val player1SkillOnServe = playerSkillsOnServeMap(r.player1)
      val player1SkillOnReturn = playerSkillsOnReturnMap(r.player1)

      val player2SkillOnServe = playerSkillsOnServeMap(r.player2)
      val player2SkillOnReturn = playerSkillsOnReturnMap(r.player2)

      addTennisMatchToFactorGraph(player1SkillOnServe.varId, player1SkillOnReturn.varId,
        player2SkillOnServe.varId, player2SkillOnReturn.varId, game)
    }

  }

  private def addTennisMatchToFactorGraph(player1OnServeVarId: Int, player1OnReturnVarId: Int,
    player2OnServeVarId: Int, player2OnReturnVarId: Int, game: Game) {

    val player1VarId = nextVarId.getAndIncrement()
    val player2VarId = nextVarId.getAndIncrement()
    val outcomeVarId = nextVarId.getAndIncrement()

    val player1Factor = PlayerFactor(player1OnServeVarId, player1OnReturnVarId, player1VarId)
    val player2Factor = PlayerFactor(player2OnServeVarId, player2OnReturnVarId, player2VarId)

    val matchFactor = new CachedTennisMatchFactor(player1Factor, player2Factor, outcomeVarId, perfVariance, perfVariance, game)

    factorGraph.addFactor(player1Factor)
    factorGraph.addFactor(player2Factor)
    factorGraph.addFactor(matchFactor)

  }

  def calibrate(iterNum: Int): EPSummary = {
    val epCalibrate = ForwardBackwardEPCalibrate(factorGraph)
    val epSummary = epCalibrate.calibrate(iterNum, currIter => {})
    epSummary
  }

  /**
   * Returns gaussian process of player skills
   */
  def getPlayerGPOnServeMarginal(player: String): CanonicalGaussian = {
    val skillGPFactor = playerGPSkillsOnServeMap(player)

    val ep = GenericEP(factorGraph)
    val skillGPMarginal = ep.marginal(skillGPFactor.varId).asInstanceOf[MvnGaussianFactor]

    skillGPMarginal.canonGaussian
  }

  def getPlayerGPOnReturnMarginal(player: String): CanonicalGaussian = {
    val skillGPFactor = playerGPSkillsOnReturnMap(player)

    val ep = GenericEP(factorGraph)
    val skillGPMarginal = ep.marginal(skillGPFactor.varId).asInstanceOf[MvnGaussianFactor]
    skillGPMarginal.canonGaussian
  }

}