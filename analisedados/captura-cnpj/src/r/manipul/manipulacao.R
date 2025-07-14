# SETUP PROJETO ----
# source("config/setup.R")
# dependencias
source("src/r/manipul/join_base_and_request.R")

# SETUP PACKAGES ----
use("tidyverse", c("mutate", "case_when"))

# Manipulação da BASE

base_mcti_request <-
base_mcti_request |>
  dplyr::mutate(
    regiao_dispendio = dplyr::case_when(
      uf.parceira %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      uf.parceira %in% c("PR", "SC", "RS") ~ "Sul",
      uf.parceira %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf.parceira %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf.parceira %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      TRUE ~ "Outra Região"
    )
  )

head(base_mcti_request$uf.parceira)
