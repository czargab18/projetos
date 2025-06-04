# Configurando outros repositórios.
source("src/r/manipul/join_base_and_request.R")

library(dplyr)
library(ggplot2)
library(dplyr)

plot_tipo <-
base_mcti_request |>
  dplyr::count(tipo_dispendio) |>
  dplyr::mutate(tipo_dispendio = factor(tipo_dispendio, levels = tipo_dispendio[order(n)])) |>
  ggplot(aes(x = tipo_dispendio, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


plot_tipo +
    labs(
      title = "Tipos de Parcerias em projetos de P,D&I",
      y = "Quantidade de Projetos"
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


names(base_mcti_request)
