# CONFIGURAÇÕES INICIAIS ----
#> configuração do projeto, pacotes e diretórios
source("config/setup.R")
#> carregar e unir as bases de dados
source("src/r/read/read_and_join_bases.R")
#> carregar funções auxiliares para gráficos
source("src/r/graficos/temas.R")

# GRÁFICO: TIPOS DE PARCERIAS ----

#plot_tipo_dispendio <-
  base_mcti_request |>
  dplyr::select(tipo_dispendio, cnpj_dispendio) |>

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
  ggplot2::geom_text(aes(label = n), color = "#000000", size = 4) +
  tema()

# GRÁFICO:
