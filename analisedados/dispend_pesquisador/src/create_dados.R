# source("config/setup.R")

# Carregar pacotes necessários
use("stringi", c("stri_rand_strings"))
use("lubridate", c("ymd", "interval", "duration"))
use("purrr", c("map", "map2", "map2_dbl"))
use("dplyr", c("mutate", "filter", "select"))
use("writexl", c("write_xlsx"))

# Função para gerar CNPJs aleatórios
gerar_cnpj <- function(n) {
  replicate(n, paste0(sample(0:9, 14, replace = TRUE), collapse = ""))
}

# Função para gerar nomes aleatórios
gerar_nome <- function(n) {
  replicate(
    n,
    stringi::stri_rand_strings(1, sample(5:10, 1), pattern = "[A-Za-z]")
  )
}

# Função para gerar IDs de projetos aleatórios
gerar_id_projeto <- function(n) {
  replicate(n, paste0("PROJ_", sample(1000:9999, 1, replace = TRUE)))
}

# Função para gerar datas aleatórias
gerar_data <- function(n, anos) {
  as.Date(sample(
    seq(Sys.Date() - anos * 365, Sys.Date(), by = "day"),
    n,
    replace = TRUE
  ))
}

# Gerar o data.frame fictício com muitos registros
n_registros <- 1000

dados <- tibble(
  cnpj = sample(gerar_cnpj(n_registros), size = n_registros, replace = TRUE),
  razao_social = sample(
    gerar_nome(n_registros),
    size = n_registros,
    replace = TRUE
  ),
  pesquisador = gerar_nome(n_registros),
  projeto = gerar_id_projeto(n_registros),
  data_contratacao = gerar_data(n_registros, anos = 1),
  data_encerramento = gerar_data(n_registros, anos = 2)
) |>
  dplyr::mutate(
    periodo = purrr::map2_dbl(
      .x = data_encerramento,
      .y = data_contratacao,
      .f = ~ round(
        lubridate::time_length(
          lubridate::interval(lubridate::ymd(.x), lubridate::ymd(.y)),
          "months"
        ),
        0
      )
    )
  ) |>
  # O valor em "periodo" representa a quantidade de meses entre a data de encerramento e a data de contratação
  # filtrando os dados que forão gerados. Ignorando o qe não faz sentido
  dplyr::filter(
    periodo >= 0
  ) |>
  # Calculo:
  #   Se periodo >= 12, um ano trabalhando no projeto => RECEBE incentivo
  #   Se periodo <= 12, um ano trabalhando no projeto => NÃO RECEBE incentivo
  dplyr::mutate(
    incentivo = dplyr::case_when(
      periodo >= 12 ~ "RECEBE",
      periodo <= 12 ~ "NÃO RECEBE",
      TRUE ~ NA
    )
  ) |>
  dplyr::rename_with(
    .fn = ~"meses",
    .cols = periodo
  )

if (!dir.exists("./data")) {
  dir.create("./data")
}

# salvando
writexl::write_xlsx(
  x = dados,
  path = "data/dados_gerados.xlsx"
)
