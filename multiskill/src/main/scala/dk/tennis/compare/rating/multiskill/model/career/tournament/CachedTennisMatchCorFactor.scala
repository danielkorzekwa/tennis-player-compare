package dk.tennis.compare.rating.multiskill.model.career.tournament

import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factor.MvnGaussianFactor
import dk.bayes.model.factor.MvnGaussianFactor

case class CachedTennisMatchCorFactor(player1: String, player2: String, servicePointsWon: Int, servicePointsTotal: Int,
  allPlayers: IndexedSeq[String], skillsFactor: MvnGaussianFactor, outcomeVarId: Int,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double)
  extends TennisMatchCorFactor(player1, player2, servicePointsWon, servicePointsTotal,
    allPlayers, skillsFactor, outcomeVarId,
    perfVarianceOnServe, perfVarianceOnReturn) {

  /**caching*/
  val skillCachingDelta = 0.000000001
  var prevSkills: Option[Factor] = None

  var cachedMsgs: Tuple2[MvnGaussianFactor, SingleTableFactor] = _

  override def outgoingMessages(skills: Factor, outcomeFactor: Factor): Tuple2[MvnGaussianFactor, SingleTableFactor] = {
    if (!prevSkills.isDefined || !prevSkills.get.equals(skills, skillCachingDelta)) {

      prevSkills = Some(skills)
      cachedMsgs = super.outgoingMessages(skills, outcomeFactor)
    }

    cachedMsgs
  }
}