package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel

class MatchLogLikTest {

  private val pointP1Wins = List.fill(70)(PointResult("player1", true))
  private val pointP1Lose = List.fill(30)(PointResult("player1", false))

  private val pointP2Wins = List.fill(64)(PointResult("player2", true))
  private val pointP2Lose = List.fill(36)(PointResult("player2", false))
  private val pointResults = pointP1Wins ::: pointP1Lose ::: pointP2Wins ::: pointP2Lose

  private val matchResult = MatchResult("player1", "player2", pointResults, true, 2)

  private val p1OnServePointProb = 0.6418324017261012
  private val p2OnServePointProb = 0.5731798196775812

  @Test def logLik {

    assertEquals(-0.4004, MatchLogLik.logLik(p1MatchProb = 0.67, true), 0.0001)
    assertEquals(-1.1086, MatchLogLik.logLik(p1MatchProb = 0.67, false), 0.0001)
  }

  @Test def logLikByPoint {

    val logLik = MatchLogLik.logLikByPoint(p1OnServePointProb, p2OnServePointProb, matchResult)

    assertEquals(-128.1122, logLik, 0.0001)
  }
}