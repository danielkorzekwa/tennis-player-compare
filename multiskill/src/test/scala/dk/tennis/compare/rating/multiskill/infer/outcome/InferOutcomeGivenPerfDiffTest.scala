package dk.tennis.compare.rating.multiskill.infer.outcome

import org.junit._
import Assert._
import scala.math._
import dk.bayes.math.gaussian.Gaussian

class InferOutcomeGivenPerfDiffTest {

  @Test def loglik {
    assertEquals(0.5, exp(InferOutcomeGivenPerfDiff.loglik(Gaussian(0, 0.7), true)), 0.0001)
    assertEquals(0.5, exp(InferOutcomeGivenPerfDiff.loglik(Gaussian(0, 0.7), false)), 0.0001)

    assertEquals(0.5944, exp(InferOutcomeGivenPerfDiff.loglik(Gaussian(0.2, 0.7), true)), 0.0001)
    assertEquals(1 - 0.5944, exp(InferOutcomeGivenPerfDiff.loglik(Gaussian(0.2, 0.7), false)), 0.0001)
  }

  @Test def loglikD {
    assertEquals(1.2101, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0, 0.7), true, muD = 0.2, varD = 0.4)), 0.0001)
    assertEquals(0.8263, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0, 0.7), false, muD = 0.2, varD = 0.4)), 0.0001)

    assertEquals(1.1177, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0.2, 0.7), true, muD = 0.2, varD = 0.4)), 0.0001)
    assertEquals(0.8493, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0.2, 0.7), false, muD = 0.2, varD = 0.4)), 0.0001)

    assertEquals(1, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0.2, 0.7), true, muD = 0.0, varD = 0.0)), 0.0001)
    assertEquals(1, exp(InferOutcomeGivenPerfDiff.loglikD(Gaussian(0.2, 0.7), false, muD = 0.0, varD = 0.0)), 0.0001)
  }

}