# Visual language shared with Aydin's other Shiny GUIs (Reporting Protocols and
# the Reliability of Science; Truth and Conformity on Networks): bootswatch 3
# "Paper" chrome, and ggplot2 theme_minimal at text size 16 with the palette
# orangered2 / #3475BC / black. Every plot in the app draws from PI_COL and
# applies theme_pi(); no color literals elsewhere.

PI_COL <- list(
  orange = "orangered2",   # misperception term; attitude not-x; S3
  blue   = "#3475BC",      # structure term; attitude x; S2-pure
  black  = "black",        # gap; declarations; S1; prediction-style baseline
  blue_light = "#9DC3E6",  # S2-mixed
  grey   = "grey55",       # reference lines
  grey_light = "grey85"
)

PI_CLASS_COL <- c("S1" = PI_COL$black, "S2-pure" = PI_COL$blue, "S2-mixed" = PI_COL$blue_light, "S3" = PI_COL$orange)

PI_BAND_STYLE <- "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px"

theme_pi <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right",
      legend.spacing.x = grid::unit(10, "pt"),
      legend.spacing.y = grid::unit(30, "pt"),
      legend.text = ggplot2::element_text(size = base_size, margin = ggplot2::margin(t = 5, b = 5, unit = "pt")),
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10, unit = "pt"), lineheight = 1.15),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, colour = "#666666", size = base_size - 3),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, unit = "pt")),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20, unit = "pt")),
      text = ggplot2::element_text(size = base_size)
    )
}
