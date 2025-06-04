# # LIMPNDO OS NOMES COM JANITOR
BASE <-
  BASE |>
  janitor::clean_names()

#  [1] "ano_base"                        "cnpj"
#  [3] "razao_social"                    "codigo_atividade_economica_ibge"
#  [5] "uf"                              "numero_projeto"
#  [7] "projeto"                         "setor"
#  [9] "tipo_dispendio"                  "cnpj_dispendio"
# [11] "razao_social_dispendio"          "servico"
# [13] "valor"

# SALVANDO BASE DE DADOS COM OS CNPJS QUE ATENDEM AS CONDIÇÕES
writexl::write_xlsx(CONDICAO_BASE, "data/processed/CONDICAO_BASE.xlsx")