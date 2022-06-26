
#' Get Play-by-Play Data
#'
#' Get the play-by-play data from collegefootballdata.com for a specified range of
#' years or weeks.
#' @param start_week int. The first week of the season you want pbp data for.
#' @param end_week int. The last week of the season you want pbp data for.
#' @param start_year int. The first season you want pbp data for.
#' @param end_year int. The last season you want pbp data for.
#' @return tibble of play-by-play data
#' @export
get_plays <- function(start_week = 1, end_week = 1, start_year = 2020, end_year = 2020){

  base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
  start_week <- start_week
  end_week <- end_week
  start_year <- start_year
  end_year <- end_year

  plays.master = dplyr::tibble()
  for (j in start_year:end_year) {
    for (i in start_week:end_week) {
      cat('Loading Plays', j, 'Week', i, '\n')
      full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
      full_url_plays_encoded <- utils::URLencode(full_url_plays) # If there are spaces in query, formats them correctly
      plays <- cfbd_api(full_url_plays_encoded, my_key())
      plays$week = i
      plays$year = j
      plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
  }

  # Rename columns to match historic
  plays.master.temp <- plays.master %>%
    dplyr::rename(minutes = clock.minutes,
                  seconds = clock.seconds) %>%
    dplyr::select(-wallclock)

  plays.master <- plays.master.temp

  rm(plays.master.temp)

  # Return dataframe

  message("Done")
  return(plays.master)
}

# Get games from the cfbd api
#'
#' Get the game data from collegefootballdata.com for a specified range of
#' years or weeks. Note that you can request future weeks for the current season
#' and you will essentially get back the schedule for the season. If the game is completed,
#' you will get the final score along with everything else.
#'
#' @param start_week int. The first week of the season you want game data for.
#' @param end_week int. The last week of the season you want game data for.
#' @param start_year int. The first season you want game data for.
#' @param end_year int. The last season you want game data for.
#' @return tibble of game-level data
#' @export
get_games <- function(start_year, end_year, start_week, end_week){

  base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

  if(missing(start_week) | missing(end_week)){

    games.master = dplyr::tibble()
    for (j in start_year:end_year) {
      cat('Loading Games for year ', j, '\n')
      full_url_games <- paste0(base_url_games, "year=", as.character(j), "&seasonType=both")
      games <- cfbd_api(full_url_games, my_key())
      games <- dplyr::as_tibble(games)
      games.master = dplyr::bind_rows(games.master, games)
    }

  } else {

    games.master = dplyr::tibble()
    for (j in start_year:end_year) {
      for (i in start_week:end_week) {
        cat('Loading Games', j, 'Week', i, '\n')
        full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
        games <- cfbd_api(full_url_games, my_key())
        games <- dplyr::as_tibble(games)
        games.master = dplyr::bind_rows(games.master, games)
      }
    }

  }



  return(games.master)

}

# Get drives from the cfbd api
#'
#' Get the drive data from collegefootballdata.com for a specified range of
#' years or weeks.
#'
#' @param start_week int. The first week of the season you want drive data for.
#' @param end_week int. The last week of the season you want drive data for.
#' @param start_year int. The first season you want drive data for.
#' @param end_year int. The last season you want drive data for.
#' @return tibble of drive-level data
#' @export
get_drives <- function(start_year, end_year, start_week, end_week){



  base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

  if(missing(start_week) | missing(end_week)){

    drives.master = dplyr::tibble()
    for (j in start_year:end_year) {
      cat('Loading drives for year', j, '\n')
      full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&seasonType=both")
      drives <- cfbd_api(full_url_drives, my_key())
      drives <- dplyr::as_tibble(drives)
      drives.master = dplyr::bind_rows(drives.master, drives)
    }
  } else {

    drives.master = dplyr::tibble()
    for (j in start_year:end_year) {
      for (i in start_week:end_week) {
        cat('Loading drives', j, 'Week', i, '\n')
        full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
        drives <- cfbd_api(full_url_drives, my_key())
        drives <- dplyr::as_tibble(drives)
        drives.master = dplyr::bind_rows(drives.master, drives)
      }
    }

  }

  return(drives.master)

}

#' Get betting data from the cfbd api
#'
#' Get the betting data from collegefootballdata.com for a specified range of
#' years or weeks. Most years contain spread data at a minimum, and recent years
#' (2021-present) have moneylines and over/unders as well.
#'
#' In the case that multiple providers have offered lines, there will be multiple entries returned
#' for each game. We've taken the liberty of taking the average line of the different
#' providers in cases where there's more than one line given. However, if you'd
#' prefer to get all the lines from the various providers yourself, you can instead
#' make a call to to betting endpoint directly (https://api.collegefootballdata.com/lines)
#' using the cfbd_api() function.
#'
#' @param start_week int. The first week of the season you want betting data for.
#' @param end_week int. The last week of the season you want betting data for.
#' @param start_year int. The first season you want betting data for.
#' @param end_year int. The last season you want betting data for.
#' @return tibble of game-level betting data
#' @export
get_betting <-
  function(start_year,
           end_year,
           start_week,
           end_week) {



    if (missing(start_year) & missing(end_year)) {
      start_year <- lubridate::year(today())
      end_year <- start_year
      warning("Setting start year and end year to ", start_year,
              " since none was provided.")
    }

    if (missing(start_year)) {
      start_year <- end_year
      warning("Setting start year to match end year ", start_year,
              " since none was provided.")
    }

    if (missing(end_year)) {
      end_year <- start_year
      warning("Setting end year to match start year ", start_year,
              " since none was provided.")
    }

    if (start_year < 2013 & end_year < 2013) {
      start_year <- 2013
      end_year <- 2013
      warning("Setting start year and end year to 2013 (earliest year data is available)")
    }

    if (start_year < 2013 & end_year >= 2013) {
      start_year <- 2013
      warning("Setting start year to 2013 (earliest year data is available)")
    }

    if (end_year < start_year) {
      stop("Start year must be less than or equal to end year")
    }

    betting.master = data.frame()

    if (missing(start_week) | missing(end_week)) {

      for (j in start_year:end_year) {
        message("Getting betting data for ", j)
        betting_url <-
          paste0("https://api.collegefootballdata.com/lines?seasonType=both&year=",
                 j)
        full_url_betting_encoded <- utils::URLencode(betting_url)
        betting <- cfbd_api(full_url_betting_encoded, my_key())
        betting <- dplyr::as_tibble(betting)
        if(nrow(betting) > 0){
          betting <- tidyr::unnest(betting, cols = c(lines))
        }
        betting.master = rbind(betting.master, betting)
      }

    } else {

      for (j in start_year:end_year) {
        for (i in start_week:end_week) {
          message("Getting betting data for ", j, " Week ", i)
          betting_url <-
            paste0("https://api.collegefootballdata.com/lines?seasonType=both&year=",
                   j,
                   "&")
          full_url_betting <- paste0(betting_url, "week=", as.character(i))

          full_url_betting_encoded <- utils::URLencode(full_url_betting)
          betting <- cfbd_api(full_url_betting_encoded, my_key())
          betting <- dplyr::as_tibble(betting)
          if(nrow(betting) > 0){
            betting <- tidyr::unnest(betting, cols = c(lines))
          }
          betting.master = rbind(betting.master, betting)
        }

      }

    }



    # Do some stuff that needs to be done eventually anyway
    betting.master <- betting.master %>%
      dplyr::group_by(id, homeTeam, awayTeam, season, week, seasonType) %>%
      dplyr::summarise(spread = mean(as.double(spread), na.rm = TRUE),
                spreadOpen = mean(as.double(spreadOpen), na.rm = TRUE),
                overUnder = mean(as.double(overUnder), na.rm = TRUE),
                overUnderOpen = mean(as.double(overUnderOpen), na.rm = TRUE),
                homeMoneyline = mean(homeMoneyline, na.rm = TRUE),
                awayMoneyline = mean(awayMoneyline, na.rm = TRUE),
                .groups = "keep"
      ) %>%
      dplyr::mutate(formattedSpread = paste0(dplyr::if_else(spread > 0, awayTeam, homeTeam),
                                      " ",
                                      "-",
                                      abs(spread))) %>%
      dplyr::ungroup()

    return(betting.master)

  }

#' Get Anything From the CFBD API
#' @description Get data from any API endpoint on cfbdata.com
#' @param url string. The url of the API endpoint WITHOUT any parameters added onto the end.
#' If you have any parameters to add, please use the provided fields and we will add them
#' to the url for you.
#' @param start_year int first year of data to return
#' @param end_year int last year of data to return
#' @param start_week int first week of data to return
#' @param end_week int last week of data to return
#' @param key your API key. Use variable my_key() or make a call to Sys.getenv("cfbd_staturdays_key")
#' @export
get_anything <- function(url, start_year=2021, end_year=2021, start_week, end_week, key=my_key()){



  if(missing(key)){

    message("You must supply an API key. You can get one for free at collegefootballdata.com")

  } else if(stringr::str_detect(url, "\\?") == TRUE){

    message("Please remove any parameters and the '?' from your url field")

  } else if(missing(start_week) & missing(end_week)){

    response <- tibble()
    for(yr in start_year:end_year){

      response_url <- paste0(url, "?year=", as.character(yr))
      r1 <- cfbd_api(response_url, key = key)
      response <- rbind(response, r1)
      message("Done year ", yr)

    }
  } else {

    response <- tibble()
    for(yr in start_year:end_year){
      for(wk in start_week:end_week){

        response_url <- paste0(url,
                               "?year=", as.character(yr),
                               "&week=", as.character(wk))
        r1 <- cfbd_api(response_url, key = key)
        response <- rbind(response, r1)
        message("Done year ", yr, " week ", wk)

      }

    }

  }

  return(response)

}

#' Get Team Colors, Mascots and Abbreviations
#' @description Get a list of each school's primary, secondary colors, mascots,
#' team nicknames, abbreviations, and more from the teams endpoint.
#' @export
get_colors <- function(){

  team_colors <- get_anything(url = "https://api.collegefootballdata.com/teams",
                              key = my_key())

  team_colors <- team_colors %>%
    tidyr::unnest(cols = logos) %>%
    dplyr::mutate(logo_color = dplyr::if_else(stringr::str_detect(logos, "dark"), "dark", "light")) %>%
    tidyr::pivot_wider(names_from = logo_color, values_from = logos)

  return(team_colors)

}

#' Get Historic Elo Ratings
#' Get Elo ratings from any year from 2000 to present.
#' @param start_year int the first year of elo ratings you want
#' @param end_year int the last year of elo ratings you want
#' @param teams character (optional) a list of teams you want the ratings for.
#' When omitted returns all teams.
#' @export
get_elo <- function(start_year = 2021, end_year = 2021, teams){

  elo <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv", encoding = "UTF-8")

  elo <- elo %>%
    dplyr::filter(season >= start_year & season <= end_year)

  if(missing(teams) == FALSE){
    elo <- elo %>% dplyr::filter(team %in% teams)
  }

  return(elo)

}

