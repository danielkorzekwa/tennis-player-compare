package dk.tennis.compare.rating.multiskill.matchloader

import java.util.Date
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

case class TournamentResult(tournamentTime: Date, tournamentName: String,players:Seq[String],matchResults:Seq[MatchResult])