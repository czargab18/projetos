# SETUP IMPORT base_mcti ----

use("readxl", c("read_xlsx"))
library(readxl)

base_mcti <-
  readxl::read_xlsx(
    path = "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj/data/processed/base_cnpjs.xlsx",
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
