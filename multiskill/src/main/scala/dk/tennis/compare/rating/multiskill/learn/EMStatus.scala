package dk.tennis.compare.rating.multiskill.learn

import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams

/**
 * @param loglik (total,avg)
 */
case class EMStatus(currIter:Int,currParams:MultiSkillParams,loglik:Tuple2[Double,Double])