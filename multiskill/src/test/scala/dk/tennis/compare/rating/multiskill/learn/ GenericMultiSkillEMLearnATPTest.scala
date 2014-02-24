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
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader

class GenericMultiSkillEMLearnATPTest {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  @Ignore @Test def test_atp_results_2011 {
      
    val multiSkillParams = MultiSkillParams(
    skillOnServeTransVariance = 0.07862661243106206,
    skillOnReturnTransVariance = 0.02358819766986756,
    priorSkillOnServe = PlayerSkill(4.015019948131644, 3.7045856424057426), priorSkillOnReturn = PlayerSkill(-2.4631217839420017, 2.832118730884739),
    perfVarianceOnServe = 200, perfVarianceOnReturn = 200)
          
    val atpFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
    val tournaments = MatchesLoader.loadTournaments(atpFile, 2006, 2011)

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, tournaments, maxIter = 30000, emProgress)

  }

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}