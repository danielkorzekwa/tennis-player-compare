package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.sim.GenericMatchSim
import scala.util.Random

class GenericMultiSkillEMLearnTest {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))
  @Test def test_empty_matches {

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 1,
      skillOnReturnTransVariance = 2,
      priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
      perfVarianceOnServe = 3, perfVarianceOnReturn = 4)

    val results = Nil

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, results, maxIter = 5)
    assertMultiSkillParams(learnedParams, multiSkillParams, 0.0001)
  }

  @Test def one_hundred_matches_two_players {

    val p1Skills = PlayerSkills("player1", PlayerSkill(2.0, 1), PlayerSkill(-2, 1))
    val p2Skills = PlayerSkills("player2", PlayerSkill(5, 1), PlayerSkill(-3, 1))

    val matchSim = GenericMatchSim(p1Skills, p2Skills, perfVarianceOnServe = 100, perfVarianceOnReturn = 100,
      random = new Random(), pointsNum = 100)

    val matchResults = List.fill(50)(matchSim.sample())
    println("Tennis matches=" + matchResults.size)

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 0.04,
      skillOnReturnTransVariance = 0.04,
      priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
      perfVarianceOnServe = 100, perfVarianceOnReturn = 100)

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, matchResults, maxIter = 3, emProgress)

  }

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}