#' Get Staturdays Colors and Themes to Build Beautiful Visuals
#' @export
staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)

#' @export
staturdays_palette <- c("#5c6272", "#ffffff", "#de703b")

#' @export
staturdays_ramp <- function(x) rgb(colorRamp(c(staturdays_palette))(x), maxColorValue = 255)

#' @export
staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

#' Create ggplot2 Objects With Staturdays Theme
#' @export
staturdays_theme <- ggplot2::theme(plot.caption = ggplot2::element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = ggplot2::element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = ggplot2::element_text(color = staturdays_colors("light_blue"), size = 20),
                          axis.text = ggplot2::element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          axis.title = ggplot2::element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.title = ggplot2::element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.text = ggplot2::element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          panel.background = ggplot2::element_blank(),
                          panel.grid = ggplot2::element_blank(), # no gridlines. If you want them, use element_line()
                          panel.grid.minor = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_line(color = "#d6d6d6")
)
