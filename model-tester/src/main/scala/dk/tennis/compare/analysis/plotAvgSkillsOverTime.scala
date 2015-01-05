package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import java.util.Date
import breeze.plot.Figure
import breeze.plot._
import breeze.plot.Figure
import breeze.plot._

object plotAvgSkillsOverTime {

  def apply(scores: Seq[Score], players: Set[String], model: InferSkill) {

    val filteredScores = scores.filter(s => players.contains(s.player1.playerName))

    val PERIOD = 1000L*3600*24*30
    val skillsByTimeStamp = filteredScores.map(s => model.inferSkill(s.player1)).groupBy(s => s.player.timestamp.getTime/PERIOD)
    val avgSkills: Seq[Tuple2[Date, Double]] = skillsByTimeStamp.map{case (time,skills) => new Date(time*PERIOD) ->skills.map(s => s.skill.m).sum / skills.size}.toList.sortBy(e => e._1)

    val plot = TimePlot(legend = false)
    plot.add(avgSkills, "")
  }
}