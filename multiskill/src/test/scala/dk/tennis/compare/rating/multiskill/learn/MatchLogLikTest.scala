package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerStats

class MatchLogLikTest {

  private val matchResult = MatchResult("player1", "player2", true, 2, PlayerStats(5,70, 100), PlayerStats(7,64, 100))

  private val p1OnServePointProb = 0.6418324017261012
  private val p2OnServePointProb = 0.5731798196775812

  @Test def logLik {

    assertEquals(-0.4004, MatchLogLik.logLik(p1MatchProb = 0.67, true), 0.0001)
    assertEquals(-1.1086, MatchLogLik.logLik(p1MatchProb = 0.67, false), 0.0001)
  }

}