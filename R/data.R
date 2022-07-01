#' List of power five conference names historically.
#'
#' @format a character vector
#'
#' @source created
"power_5"

#' List of group of five conference names historically.
#'
#' @format a character vector
#'
#' @source created
"group_of_5"

#' List of all scrimmage play types
#'
#' This contains the play names typically used in the pbp
#' data for scrimmage plays. This does not include special teams plays
#' like kickoffs or punts.
#'
#' @format a character vector:
#'
#'
#' @source cfbd pbp data
"scrimmage_plays_all"

#' List of all plays that don't result in a turnover
#'
#' @format a character vector
#'
#' @source created
"scrimmage_plays_non_turnover"

#' List of all plays that DO result in a turnover
#'
#' This contains play types such as interceptions and fumbles.
#' This does NOT include punts.
#'
#' @format a character vector
#'
#' @source created
"scrimmage_plays_turnover"

#' List of all passing play types
#'
#' @format a character vector
#'
#' @source created
"scrimmage_plays_pass"

#' List of all rushing play types
#'
#' @format a character vector
#'
#' @source created
"scrimmage_plays_rush"

#' List of all play types where no play occurs (timeouts, end of quarters, ...)
#'
#' @format a character vector
#'
#' @source created
"no_action_plays"

#' List of all plays that result in a kick
#'
#' @format a character vector
#'
#' @source created
"scrimmage_plays_kicks"

#' List of the staturdays color palette
#'
#' @format a character vector
"staturdays_palette"

#' List of the staturdays colors
#'
#' @format a character vector
"staturdays_col_list"

#' ggplot2 theme for use with plotting
#'
#' This sets some useful defaults to make plots look more polished.
#'
#' To use it, simply put this line at the end of your ggplot code:
#' + staturdays_theme
#'
#' Note: This is not a function. Don't use ().
#'
#' @format ggplot2 theme() object.
"staturdays_theme"

