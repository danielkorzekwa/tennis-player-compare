package dk.tennis.compare.rating.multiskill.model.od.utils

import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.od.Game

/**
 * points [pointsWon,pointsTotal]
 */
case class Prediction(game:Game, player1:String,player1Skill:PlayerSkill,player2:String,player2Skill:PlayerSkill,pointProb: Double, points: Tuple2[Int, Int])