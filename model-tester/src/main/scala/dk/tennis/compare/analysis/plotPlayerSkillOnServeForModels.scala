package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import breeze.plot.Figure
import breeze.plot._
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.infer.skills.givenallmatches.InferSkillGivenAllMatches
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import dk.tennis.compare.rating.multiskill.infer.skills.givenmatchesloo.InferSkillGivenMatchesLoo
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import org.joda.time.DateTime
import java.util.Date
import java.text.NumberFormat
import java.text.FieldPosition
import java.text.ParsePosition
import java.text.SimpleDateFormat

object plotPlayerSkillOnServeForModels extends Logging {

  def apply(scores: Seq[Score], players: Seq[String], models: Map[String, InferSkill], legend: Boolean) {

    val plot = TimePlot(legend)

    players.foreach { p =>
      val filteredScores = scores.filter(s => s.player1.playerName.equals(p))

      models.foreach {
        case (modelName, model) =>

          val skills = filteredScores.map(s => s.player1.timestamp -> model.inferSkill(s.player1).skill.m)

          plot.add(skills, label = p + ":" + modelName)
        //   plotSkills(f, skills, p + ":" + modelName,legend)
      }
      filteredScores.zipWithIndex.foreach { case (score, index) => println((index + 1) + ":" + score.player1) }
    }

  }

}