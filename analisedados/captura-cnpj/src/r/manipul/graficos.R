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
  title = "União Federativa dass parcerias em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)


regiao_dispendio <-
base_mcti_request |>
  dplyr::count(regiao_dispendio) |>
  dplyr::mutate(regiao_dispendio = factor(regiao_dispendio, levels = regiao_dispendio[order(n)])) |>
  ggplot(aes(x = regiao_dispendio, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

theme_custom(
  regiao_dispendio,
  title = "Regiao dos parceiros em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)

# PRINCIPAIS FUNDAÇÕES  ----
# tipo de fundação publica
base_mcti_request |>
  dplyr::select(natureza_juridica, razao_social.y) |>
  dplyr::filter(
    stringr::str_detect(natureza_juridica, "Fundação Pública")
  ) |>
  dplyr::count(razao_social.y) |>
  dplyr::arrange(desc(n)) |>
  View()


# nomes da fundação (publica ou privada)
base_mcti_request |>
  dplyr::select(razao_social.y) |>
  dplyr::filter(
    stringr::str_detect(razao_social.y, "FUNDACAO (UNIVERSIDADE|FACULDADE)")
  ) |>
  dplyr::count(razao_social.y) |>
  dplyr::arrange(desc(n))  |>
  View()
