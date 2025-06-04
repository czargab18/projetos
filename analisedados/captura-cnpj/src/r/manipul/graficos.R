# Configurando outros repositórios.
source("src/r/manipul/join_base_and_request.R")
source("src/r/manipul/manipulacao.R")

library(dplyr)
library(ggplot2)
library(dplyr)

# FUNCTIONS ----
theme_custom <- function(plot, title, title.y) {
  plot +
    labs(
      title = title,
      y = title.y
    ) +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.x = element_line(color = "#c0bebe", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x.bottom = element_line(color = "black")
    )
}

# GRÁFICOS ----

names(base_mcti_request)

plot_tipo_dispendio <-
base_mcti_request |>
  dplyr::count(tipo_dispendio) |>
  dplyr::mutate(tipo_dispendio = factor(tipo_dispendio, levels = tipo_dispendio[order(n)])) |>
  ggplot(aes(x = tipo_dispendio, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

theme_custom(
  plot_tipo_dispendio,
  title = "Tipos de Parcerias em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)

uf_dispendio <-
base_mcti_request |>
  dplyr::count(uf.y) |>
  dplyr::mutate(uf.y = factor(uf.y, levels = uf.y[order(n)])) |>
  ggplot(aes(x = uf.y, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

theme_custom(
  plot_tipo,
  title = "Tipos de Parcerias em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)
