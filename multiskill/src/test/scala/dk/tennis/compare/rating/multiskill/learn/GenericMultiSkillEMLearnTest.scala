package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.util.Random
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import java.util.Date

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

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}