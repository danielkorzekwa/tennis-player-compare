package dk.tennis.compare.rating.multiskill.testutil

import org.junit.Assert.assertEquals

import dk.bayes.math.gaussian.Gaussian

object MultiSkillTestUtil {

   def assertGaussian(expected: Gaussian, actual: Gaussian, delta: Double) = {
    assertEquals(expected.m, actual.m, delta)
    assertEquals(expected.v, actual.v, delta)
  }
  
}