# SETUP PROJETO ----
# source("config/setup.R")


# SETUP IMPORT base_mcti ----
base_mcti <-
  readxl::read_xlsx(
    path = "./data/processed/mcti_base_cnpjs.xlsx",
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