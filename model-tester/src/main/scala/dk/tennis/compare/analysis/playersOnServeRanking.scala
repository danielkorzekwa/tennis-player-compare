package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.perfdiff.NumOfSets

object playersOnServeRanking {

  def apply(players: Seq[String], inferSkill: InferSkill, timestamp: Date): Seq[PlayerSkill] = {

    val skills = players.map { p =>
      val playerOnServe = Player(p, p, true, timestamp, Surface.HARD, NumOfSets.THREE_SETS)
      val skillOnServe = inferSkill.inferSkill(playerOnServe)

      skillOnServe
    }.sortBy(s => s.skill.m)

    skills
  }
}