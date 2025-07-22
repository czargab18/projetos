# SETUP PROJETO ----
source("config/setup.R")

# IMPORTAR BASE MCTI ----
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

# MENSAGEM DE CARREGAMENTO ----
print(paste("Base MCTI carregada com", nrow(base_mcti), "linhas."))


# IMPORTAR BASE REQUESIÇÃO ----

base_request <-
  read_delim(
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

# MENSAGEM DE CARREGAMENTO ----
print(paste("Base Requisição carregada com", nrow(base_request), "linhas."))

# JUNTAR BASES ----
base_mcti_request <-
  dplyr::right_join(
    x = base_mcti,
    y = base_request,
    by = c("cnpj_dispendio" = "cnpj"),
    suffix = c(".x", ".y")
  ) |>
  dplyr::mutate_all(as.character) |>
  dplyr::rename_with(~ gsub("\\.x$", ".empresa", .x)) |>
  dplyr::rename_with(~ gsub("\\.y$", ".parceira", .x)) |>
  dplyr::mutate(
    regiao_dispendio = dplyr::case_when(
      uf.parceira %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      uf.parceira %in% c("PR", "SC", "RS") ~ "Sul",
      uf.parceira %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf.parceira %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~
        "Nordeste",
      uf.parceira %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      TRUE ~ "EXTERIOR"
    ),
    uf_comp = dplyr::case_when(
      uf.parceira == "AC" ~ "Acre",
      uf.parceira == "AL" ~ "Alagoas",
      uf.parceira == "AP" ~ "Amapá",
      uf.parceira == "AM" ~ "Amazonas",
      uf.parceira == "BA" ~ "Bahia",
      uf.parceira == "CE" ~ "Ceará",
      uf.parceira == "DF" ~ "Distrito Federal",
      uf.parceira == "ES" ~ "Espírito Santo",
      uf.parceira == "GO" ~ "Goiás",
      uf.parceira == "MA" ~ "Maranhão",
      uf.parceira == "MT" ~ "Mato Grosso",
      uf.parceira == "MS" ~ "Mato Grosso do Sul",
      uf.parceira == "MG" ~ "Minas Gerais",
      uf.parceira == "PA" ~ "Pará",
      uf.parceira == "PB" ~ "Paraíba",
      uf.parceira == "PR" ~ "Paraná",
      uf.parceira == "PE" ~ "Pernambuco",
      uf.parceira == "PI" ~ "Piauí",
      uf.parceira == "RJ" ~ "Rio de Janeiro",
      uf.parceira == "RN" ~ "Rio Grande do Norte",
      uf.parceira == "RS" ~ "Rio Grande do Sul",
      uf.parceira == "RO" ~ "Rondônia",
      uf.parceira == "RR" ~ "Roraima",
      uf.parceira == "SC" ~ "Santa Catarina",
      uf.parceira == "SP" ~ "São Paulo",
      uf.parceira == "SE" ~ "Sergipe",
      uf.parceira == "TO" ~ "Tocantins",
      TRUE ~ NA_character_
    ),
    valor = as.numeric(str_trim(format(valor, decimal.mark = ",", nsmall = 2)))
  )

# MENSAGEM DE CARREGAMENTO ----
print(paste(
  "Base MCTI + Requisição carregada com",
  nrow(base_mcti_request),
  "linhas."
))

# SALVANDO BASE COMPLETA ----

# Salvando base de dados MCTI
writexl::write_xlsx(base_mcti, "data/processed/base_mcti.csv")

# Salvando base de dados de REQUISIÇÃO
write.table(
  x = base_request,
  file = "data/processed/base_request.csv",
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)

# Salvando base de dados MCTI + REQUISIÇÃO
utils::write.table(
  x = base_mcti_request,
  file = "data/processed/base_mcti_request.csv",
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  fileEncoding = "UTF-8"
)



