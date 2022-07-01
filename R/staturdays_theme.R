#' Create a color ramp
#'
#' @description This can be used for gt() tables if you want
#' a gradient from one color to another representing low-to-high values.
#'
#' @param x a numeric value that you want to convert to a color along a scale from
#' min being the first color in the staturdays palette to max being the last.
#'
#' @export
staturdays_ramp <- function(x) rgb(colorRamp(c(staturdays_palette))(x), maxColorValue = 255)

#' Get a list of staturdays colors and hexes
#'
#' @param ... either NULL or a character vector of colors in the palette you wish
#' to return hex values for.
#'
#' @description Simply call staturdays_colors() to get a list of our
#' theme colors and their hex values, or provide a color name and it will
#' return the hex.
#'
#' @export
staturdays_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (staturdays_col_list)

  staturdays_col_list[cols]
}
