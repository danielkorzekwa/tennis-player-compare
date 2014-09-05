package dk.tennis.compare.rating.multiskill.model.perfdiff

import java.util.Date
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import scala.collection.immutable.HashSet

case class Player(playerName: String,opponentName:String, onServe: Boolean, timestamp: Date)
