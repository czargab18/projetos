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
  dplyr::mutate(
      test_detec = stringr::str_replace_all(servico, "\\s*-\\s*", " "),
      test_detec = stringr::str_replace_all(test_detec, "\\s*[\\-–]\\s*", ". "),
      test_detec = stringr::str_replace_all(test_detec, ";\\s*", ". ")
  )

#  [1] "ano_base"                        "cnpj"
#  [3] "razao_social"                    "codigo_atividade_economica_ibge"
#  [5] "uf"                              "numero_projeto"
#  [7] "projeto"                         "setor"
#  [9] "tipo_dispendio"                  "cnpj_dispendio"
# [11] "razao_social_dispendio"          "servico"
# [13] "valor"

# TEMP: ANALISE QUEBRAS DE LINHAS ----

# RESULTADOS: há ";" em serviço que pode interferir na exportação das informações
# [1] - Elaboração de documento atualizado do Estado da Arte – Uso de RV e RA em HVDC; - Preleção de Software e Hardware para criação dos s


base_mcti |>
  dplyr::filter(
    stringr::str_detect(razao_social, "CENTRAIS ELETRICAS DO NORTE DO BRASIL S/A") &
      stringr::str_detect(servico, "Elaboração de docu")
  ) |>



# SALVANDO ----
## SALVANDO BASE DE DADOS COM OS CNPJS QUE ATENDEM AS CONDIÇÕES
# writexl::write_xlsx(base_mcti, "data/processed/base_mcti.xlsx")