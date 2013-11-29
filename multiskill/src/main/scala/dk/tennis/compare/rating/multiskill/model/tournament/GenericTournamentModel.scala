package dk.tennis.compare.rating.multiskill.model.tournament

import java.util.concurrent.atomic.AtomicInteger
import dk.bayes.model.clustergraph.factor.SingleFactor
import dk.bayes.model.clustergraph.factor.Factor
import dk.bayes.model.clustergraph.factor.Var
import dk.bayes.model.clustergraph.factor.MultiFactor
import scala.annotation.tailrec

object GenericTournamentModel extends TournamentModel {

  def winningProbs(draw: Seq[Tuple2[String, String]], winProb: (String, String) => Double): Map[String, Double] = {

    require(draw.size > 0, "Tournament draw is empty")
    val lastVarId = new AtomicInteger(0)

    @tailrec
    def infer(round: Seq[RoundFactor]): RoundFactor = {

      val nextRound = round.grouped(2).map { g =>
        val pair1 = g(0)
        val pair2 = g(1)

        val nextRoundFactor = createNextRoundFactor(pair1, pair2, lastVarId.getAndIncrement, winProb)
        val factorMarginal = nextRoundFactor.product(pair1.matchFactor).product(pair2.matchFactor).marginal(nextRoundFactor.getVariables.last.id).normalise

        RoundFactor(pair1.players ++ pair2.players, factorMarginal)
      }.toSeq

      if (nextRound.size == 1) nextRound.head else infer(nextRound.toSeq)
    }

    if (draw.size == 1) {
      val p1Prob = winProb(draw.head._1, draw.head._2)
      Map(draw.head._1 -> p1Prob, draw.head._2 -> (1 - p1Prob))
    } else {

      val firstRoundFactors = draw.map { m =>
        val p1Prob = winProb(m._1, m._2)
        val matchFactor = Factor(Var(lastVarId.getAndIncrement, 2), Array(p1Prob, 1 - p1Prob))
        RoundFactor(List(m._1, m._2), matchFactor)
      }

      val finalRoundFactor = infer(firstRoundFactors)
      Map(finalRoundFactor.players.zip(finalRoundFactor.matchFactor.getValues): _*)
    }

  }

  def createNextRoundFactor(pair1: RoundFactor, pair2: RoundFactor, factorVarId: Int, winProb: (String, String) => Double): MultiFactor = {
    val nextRoundPlayers = pair1.players ++ pair2.players
    val nextRoundVarId = Var(factorVarId, nextRoundPlayers.size)
    val nextRoundProbs = for (pair1Player <- pair1.players; pair2Player <- pair2.players; nextRoundPlayer <- nextRoundPlayers) yield {

      val p1MatchProb = winProb(pair1Player, pair2Player)
      val nextRoundPlayerProb = if (nextRoundPlayer.equals(pair1Player)) p1MatchProb
      else if (nextRoundPlayer.equals(pair2Player)) 1 - p1MatchProb
      else 0d
      nextRoundPlayerProb
    }
    val nextRoundFactor = Factor(pair1.matchFactor.getVariables.last, pair2.matchFactor.getVariables.last, nextRoundVarId, nextRoundProbs.toArray)
    nextRoundFactor
  }

  case class RoundFactor(players: Seq[String], matchFactor: SingleFactor)
}