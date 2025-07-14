# SETUP PROJETO ----
# source("config/setup.R")


library(tidyverse)
library(janitor)
library(utils)

base_request <- read_delim(
  file = "./data/processed/cnpjs_transforado.csv",
  col_types = list(col_character()),
  delim = ";"
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(
      c(
        nome_fantasia,
        razao_social,
        descricao_cnae_fiscal,
        descricao_tipo_de_logradouro,
        logradouro,
        complemento,
        bairro,
        municipio,
        nome_cidade_no_exterior,
       # qsa,
        cna_es_secundarios
      ),
      ~ stringr::str_replace_all(.x, ";", "\\\\n")
    )
  )


# # SALVANDO BASE COMPLETA ----
# # usar separador ";"
# write.table(
#   x = base_request,
#   file = "data/processed/base_request.csv",
#   sep = ";",
#   row.names = FALSE,
#   col.names = TRUE,
#   quote = TRUE,
#   fileEncoding = "UTF-8"
# )
