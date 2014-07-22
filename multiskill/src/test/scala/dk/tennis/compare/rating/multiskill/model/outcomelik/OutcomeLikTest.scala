package dk.tennis.compare.rating.multiskill.model.outcomelik

import org.junit._
import Assert._
import dk.bayes.math.gaussian.Gaussian
import scala.math._

class OutcomeLikTest {

  @Test def loglik {
    assertEquals(0.5, exp(OutcomeLik.loglik(Gaussian(0, 0.7), true)), 0.0001)
    assertEquals(0.5, exp(OutcomeLik.loglik(Gaussian(0, 0.7), false)), 0.0001)

    assertEquals(0.5944, exp(OutcomeLik.loglik(Gaussian(0.2, 0.7), true)), 0.0001)
    assertEquals(1 - 0.5944, exp(OutcomeLik.loglik(Gaussian(0.2, 0.7), false)), 0.0001)
  }

  @Test def loglikD {
    assertEquals(1.2101, exp(OutcomeLik.loglikD(Gaussian(0, 0.7), true, muD = 0.2, varD = 0.4)), 0.0001)
    assertEquals(0.8263, exp(OutcomeLik.loglikD(Gaussian(0, 0.7), false, muD = 0.2, varD = 0.4)), 0.0001)

    assertEquals(1.1177, exp(OutcomeLik.loglikD(Gaussian(0.2, 0.7), true, muD = 0.2, varD = 0.4)), 0.0001)
    assertEquals(0.8493, exp(OutcomeLik.loglikD(Gaussian(0.2, 0.7), false, muD = 0.2, varD = 0.4)), 0.0001)

    assertEquals(1, exp(OutcomeLik.loglikD(Gaussian(0.2, 0.7), true, muD = 0.0, varD = 0.0)), 0.0001)
    assertEquals(1, exp(OutcomeLik.loglikD(Gaussian(0.2, 0.7), false, muD = 0.0, varD = 0.0)), 0.0001)
  }
}
