package dk.tennis.compare.rating.multiskill.model.perfdiff

import java.util.Date
import scala.collection.immutable.HashSet

case class Player(playerName: String,opponentName:String, onServe: Boolean, timestamp: Date)
