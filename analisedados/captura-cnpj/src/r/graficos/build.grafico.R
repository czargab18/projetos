# CONFIGURAÇÕES INICIAIS ----
#> configuração do projeto, pacotes e diretórios
source("config/setup.R")
#> carregar e unir as bases de dados
source("src/r/read/read_and_join_bases.R")
#> carregar funções auxiliares para gráficos
source("src/r/graficos/temas.R")

# GRÁFICO: TIPOS DE PARCERIAS ----

# plot_tipo_dispendio <-
base_mcti_request |>
  dplyr::count(tipo_dispendio) |>
  dplyr::mutate(
    tipo_dispendio = factor(
      tipo_dispendio,
      levels = tipo_dispendio[order(n)]
    )
  ) |>
  ggplot(aes(x = tipo_dispendio, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Tipos de Parcerias",
    y = "Quantidade de Projetos"
  ) +
  theme_custom2()

theme_custom(
  plot_tipo_dispendio,
  title = "Tipos de Parcerias em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)