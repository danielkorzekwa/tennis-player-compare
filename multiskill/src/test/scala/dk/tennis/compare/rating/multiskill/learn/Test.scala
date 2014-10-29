package dk.tennis.compare.rating.multiskill.learn

import dk.bayes.infer.gp.cov.CovSEiso

import scala.math._

object Test extends App{
  
  val cov = CovSEiso(log(1.6),log(0.05))

  println(cov.cov(5.99,5.92))
   println(cov.cov(5.99,5.67))
}