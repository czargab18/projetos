# install.packages("tidyverse")

library(tidyverse)


base_request <- read_delim(
  file = "./data/processed/cnpjs_data.csv",
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
        qsa,
        cna_es_secundarios
      ),
      ~ stringr::str_replace_all(.x, ";", "\\\\n")
    )
  )

base_request |>
  dplyr::filter(
    dplyr::if_any(everything(), ~ stringr::str_detect(.x, "7852005005', 'forma_de_tributacao': '"))
  ) |>
  dplyr::select(regime_tributario)  |>
  View()

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
