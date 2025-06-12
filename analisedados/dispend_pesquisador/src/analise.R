# source("src/create_dados.R")

use("lubridate", "")
use("purrr")

lubridate::dif

dados_ficticios <-
dados_ficticios |>
  dplyr::mutate(
    periodo = purrr::map2_dbl(
      .x = data_encerramento,
      .y = data_contratacao,
      .f = ~ round(
        lubridate::time_length(
          lubridate::interval(lubridate::ymd(.x), lubridate::ymd(.y)),
          "months"
        ),
        2
      )
    )
  ) |>
  # O valor em "periodo" representa a quantidade de meses entre a data de encerramento e a data de contratação
  # filtrando os dados que forão gerados. Ignorando o qe não faz sentido
  dplyr::filter(
    periodo >=0
  )  |>
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
  dplyr::filter(
    incentivo == "RECEBE"
  )

