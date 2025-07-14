library(ggplot2)

theme_custom <- function(plot, title, title.y) {
  plot +
    ggplot2::labs(
      title = title,
      y = title.y
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#c0bebe", linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x.bottom = ggplot2::element_line(color = "black")
    )
}

theme_custom2 <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = "#c0bebe", linetype = "dashed"
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(color = "black")
  )
}
