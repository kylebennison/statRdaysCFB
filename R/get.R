
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
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  
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
      plays <- cfbd_api(full_url_plays_encoded, my_key)
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

# Function to get games from cfbd api
#' @export
get_games <- function(start_year, end_year, start_week, end_week){
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
  
  base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
  
  if(missing(start_week) | missing(end_week)){
    
    games.master = dplyr::tibble()
    for (j in start_year:end_year) {
      cat('Loading Games for year ', j, '\n')
      full_url_games <- paste0(base_url_games, "year=", as.character(j), "&seasonType=both")
      games <- cfbd_api(full_url_games, my_key)
      games <- dplyr::as_tibble(games)
      games.master = dplyr::bind_rows(games.master, games)
    }
    
  } else {
    
    games.master = dplyr::tibble()
    for (j in start_year:end_year) {
      for (i in start_week:end_week) {
        cat('Loading Games', j, 'Week', i, '\n')
        full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
        games <- cfbd_api(full_url_games, my_key)
        games <- dplyr::as_tibble(games)
        games.master = dplyr::bind_rows(games.master, games)
      }
    }
    
  }
  
  
  
  return(games.master)
  
}

# Function to get drives from cfbd api
#' @export
get_drives <- function(start_year, end_year, start_week, end_week){
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
  
  base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data
  
  if(missing(start_week) | missing(end_week)){
    
    drives.master = dplyr::tibble()
    for (j in start_year:end_year) {
      cat('Loading drives for year', j, '\n')
      full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&seasonType=both")
      drives <- cfbd_api(full_url_drives, my_key)
      drives <- dplyr::as_tibble(drives)
      drives.master = dplyr::bind_rows(drives.master, drives)
    }
  } else {
    
    drives.master = tibble()
    for (j in start_year:end_year) {
      for (i in start_week:end_week) {
        cat('Loading drives', j, 'Week', i, '\n')
        full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
        drives <- cfbd_api(full_url_drives, my_key)
        drives <- dplyr::as_tibble(drives)
        drives.master = dplyr::bind_rows(drives.master, drives)
      }
    }
    
  }
  
  return(drives.master)
  
}

# Get bets
#' @export
get_betting <-
  function(start_year,
           end_year,
           start_week,
           end_week) {
    
    source(
      "https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R"
    )
    
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
        full_url_betting_encoded <- URLencode(betting_url)
        betting <- cfbd_api(full_url_betting_encoded, my_key)
        betting <- as_tibble(betting)
        if(nrow(betting) > 0){
          betting <- unnest(betting, cols = c(lines))
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
          
          full_url_betting_encoded <- URLencode(full_url_betting)
          betting <- cfbd_api(full_url_betting_encoded, my_key)
          betting <- as_tibble(betting)
          if(nrow(betting) > 0){
            betting <- unnest(betting, cols = c(lines))
          }
          betting.master = rbind(betting.master, betting)
        }
        
      }
      
    }
    
    
    
    # Do some stuff that needs to be done eventually anyway
    betting.master <- betting.master %>% 
      group_by(id, homeTeam, awayTeam, season, week, seasonType) %>% 
      summarise(spread = mean(as.double(spread), na.rm = TRUE),
                spreadOpen = mean(as.double(spreadOpen), na.rm = TRUE),
                overUnder = mean(as.double(overUnder), na.rm = TRUE),
                overUnderOpen = mean(as.double(overUnderOpen), na.rm = TRUE),
                homeMoneyline = mean(homeMoneyline, na.rm = TRUE),
                awayMoneyline = mean(awayMoneyline, na.rm = TRUE),
                .groups = "keep"
      ) %>% 
      mutate(formattedSpread = paste0(if_else(spread > 0, awayTeam, homeTeam),
                                      " ",
                                      "-",
                                      abs(spread))) %>% 
      ungroup()
    
    return(betting.master)
    
  }

#' Get Anything From the CFBD API
#' @description Get data from any API endpoint on cfbdata.com
#' @param url string. The url of the API endpoint WITHOUT any parameters added onto the end.
#' If you have any parameters to add, please use the provided fields and we will add them
#' to the url for you.
#' @export
get_anything <- function(url, start_year=2021, end_year=2021, start_week, end_week, key){
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  
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

# Get Team Colors
#' @export
get_colors <- function(){
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/get_anything.R")
  
  team_colors <- get_anything(url = "https://api.collegefootballdata.com/teams",
                              key = my_key)
  
  team_colors <- team_colors %>% unnest(cols = logos) %>% 
    mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
    pivot_wider(names_from = logo_color, values_from = logos)
  
  return(team_colors)
  
}

#' Get Historic Elo Ratings
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

