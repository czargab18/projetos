# SETUP PROJETO ----
source("config/setup.R")

# SETUP PACKAGES ----
use("readxl", c("read_xlsx"))
use("janitor", c("clean_names"))
use("dplyr", c("mutate", "filter", "select"))
use("stringr", c("str_replace_all", "str_remove", "str_detect"))
use("writexl", c("write_xlsx"))

# SETUP IMPORT base_mcti ----
base_mcti <-
  readxl::read_xlsx(
    path = "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj/data/processed/mcti_base_cnpjs.xlsx",
    sheet = "Export",
    col_types = "text"
  ) |>
  janitor::clean_names() |>
  # formata caracteres, como ";" e "-"
  dplyr::mutate(
      servico = stringr::str_replace_all(servico, "\\s*-\\s*", " "),
      servico = stringr::str_replace_all(servico, "\\s*[\\-–]\\s*", ". "),
      servico = stringr::str_replace_all(servico, ";\\s*", ". ")
  )

# SALVANDO ----
## SALVANDO BASE DE DADOS COM OS CNPJS QUE ATENDEM AS CONDIÇÕES
# writexl::write_xlsx(base_mcti, "data/processed/base_mcti.csv")
