# CONFIGURAÇÕES INICIAIS ----
#> configuração do projeto, pacotes e diretórios
source("config/setup.R")
#> carregar e unir as bases de dados
source("src/r/read/read_and_join_bases.R")
#> carregar funções auxiliares para gráficos
source("src/r/graficos/temas.R")

# GRÁFICO: TIPOS DE PARCERIAS ----
base_mcti_request |>
  select(tipo_dispendio, cnpj_dispendio) |>
  group_by(tipo_dispendio) |>
  summarise(cnpjs = list(cnpj_dispendio)) |>
  mutate(
    cnpjs = map(cnpjs, unique),
    qtd_cnpjs = lengths(cnpjs)
  )


# plot_tipo_dispendio <-
base_mcti_request |>
  dplyr::select(tipo_dispendio, cnpj_dispendio) |>
  dplyr::group_by(tipo_dispendio) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    tipo_dispendio = factor(
      tipo_dispendio,
      levels = tipo_dispendio[order(quantidade)]
    )
  ) |>
  dplyr::select(tipo_dispendio, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = tipo_dispendio, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Tipos de Parcerias",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(aes(label = quantidade), color = "#000000", size = 4) +
  tema()



# GRÁFICO:
