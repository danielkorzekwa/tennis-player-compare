package dk.tennis.compare

import dk.atp.api.tournament.GenericTournamentAtpApi
import dk.atp.api.GenericATPMatchesLoader
import dk.atp.api.CSVATPMatchesLoader

object AtpApp extends App {

  private val tournamentApi = new GenericTournamentAtpApi(20000)
  private val atpMatchesLoader = new GenericATPMatchesLoader(tournamentApi, 1)

  for (i <- 2014 to 2014) {
    val matches = atpMatchesLoader.loadMatches(i)

    CSVATPMatchesLoader.toCSVFile(matches, "match_data_" + i + ".csv")
  }

}