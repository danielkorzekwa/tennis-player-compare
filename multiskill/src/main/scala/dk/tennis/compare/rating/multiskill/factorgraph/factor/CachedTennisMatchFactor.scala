package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.od.Game

class CachedTennisMatchFactor(p1Factor: PlayerFactor, p2Factor: PlayerFactor, outcomeVarId: Int,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double, game: Game)
  extends TennisMatchFactor(p1Factor, p2Factor, outcomeVarId, perfVarianceOnServe, perfVarianceOnReturn, game) {

  /**caching*/
  val skillCachingDelta = 0.000000001
  var prevP1Skills: Option[Factor] = None
  var prevP2Skills: Option[Factor] = None
  var cachedMsgs: Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = _

  override def outgoingMessages(p1Skills: Factor, p2Skills: Factor, outcomeFactor: Factor): Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = {
    if (!prevP1Skills.isDefined || !prevP1Skills.get.equals(p1Skills, skillCachingDelta) || !prevP2Skills.get.equals(p2Skills, skillCachingDelta)) {

      prevP1Skills = Some(p1Skills)
      prevP2Skills = Some(p2Skills)
      cachedMsgs = super.outgoingMessages(p1Skills, p2Skills, outcomeFactor)
    }

    cachedMsgs
  }
}