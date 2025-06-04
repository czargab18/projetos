# SETUP IMPORT base_mcti ----

use("readxl", c("read_xlsx"))
library(readxl)

base_mcti <-
  readxl::read_xlsx(
    path = "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj/data/processed/base_cnpjs.xlsx",
    sheet = "Export",
    col_types = "text"
  ) |>
  janitor::clean_names()

#  [1] "ano_base"                        "cnpj"
#  [3] "razao_social"                    "codigo_atividade_economica_ibge"
#  [5] "uf"                              "numero_projeto"
#  [7] "projeto"                         "setor"
#  [9] "tipo_dispendio"                  "cnpj_dispendio"
# [11] "razao_social_dispendio"          "servico"
# [13] "valor"

# SALVANDO ----
## SALVANDO BASE DE DADOS COM OS CNPJS QUE ATENDEM AS CONDIÇÕES
# writexl::write_xlsx(base_mcti, "data/processed/base_mcti.xlsx")