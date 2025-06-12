# SETUP PROJETO ----
source("config/setup.R")
# dependencias
source("src/r/manipul/join_base_and_request.R")

# SETUP PACKAGES ----
use("tidyverse", c("mutate", "case_when"))

# Manipulação da BASE

base_mcti_request <-
base_mcti_request |>
  dplyr::mutate(
    regiao_dispendio = dplyr::case_when(
      uf.y %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      uf.y %in% c("PR", "SC", "RS") ~ "Sul",
      uf.y %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf.y %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf.y %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      TRUE ~ "Outra Região"
    )
  )
