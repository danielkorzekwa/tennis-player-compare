package dk.tennis.compare.rating.multiskill.model.perfdiff

import java.util.Date
import scala.collection.immutable.HashSet
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface._
import dk.tennis.compare.rating.multiskill.model.perfdiff.NumOfSets._
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

case class Player(playerName: String, opponentName: String, onServe: Boolean, timestamp: Date, surface: Surface, numOfSets: NumOfSets)
