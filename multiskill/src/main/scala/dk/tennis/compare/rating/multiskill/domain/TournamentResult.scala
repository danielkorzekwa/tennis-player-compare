package dk.tennis.compare.rating.multiskill.domain

import java.util.Date

case class TournamentResult(tournamentTime: Date, tournamentName: String,players:Seq[String],matchResults:Seq[MatchResult])