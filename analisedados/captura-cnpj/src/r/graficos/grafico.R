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


plot_tipo_dispendio <-
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
  ggplot2::geom_text(aes(label = quantidade), color = "#000000", size = 4, hjust = -0.05) +
  tema()

ggplot2::ggsave(
  plot = plot_tipo_dispendio,
  filename = "resultado/graficos/tipos_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300
)

# GRÁFICO:Unidade federativas das ICTs ----
plot_uf_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in% c("Instituição de Pesquisa", "Universidades")
  ) |>
  dplyr::select(uf.parceira, cnpj_dispendio) |>
  dplyr::group_by(uf.parceira) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    uf.parceira = factor(
      uf.parceira,
      levels = uf.parceira[order(quantidade)]
    )
  ) |>
  dplyr::select(uf.parceira, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = uf.parceira, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "União Federativa das ICTS em projetos de P,D&I",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(aes(label = quantidade), color = "#000000", size = 4, hjust = -0.05) +
  tema()

ggplot2::ggsave(
  plot = plot_uf_dispendio,
  filename = "resultado/graficos/uf_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300
)

# GRÁFICO: REGIÃO das ICTs ----
plot_regiao_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in% c("Instituição de Pesquisa", "Universidades") &
      regiao_dispendio != "EXTERIOR"
  ) |>
  dplyr::select(regiao_dispendio, cnpj_dispendio) |>
  dplyr::group_by(regiao_dispendio) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    regiao_dispendio = factor(
      regiao_dispendio,
      levels = regiao_dispendio[order(quantidade)]
    )
  ) |>
  dplyr::select(regiao_dispendio, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = regiao_dispendio, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Região das ICTs em projetos de P,D&I",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(aes(label = quantidade), color = "#000000", size = 4, hjust = -0.05) +
  tema()

ggplot2::ggsave(
  plot = plot_regiao_dispendio,
  filename = "resultado/graficos/regiao_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300
)
