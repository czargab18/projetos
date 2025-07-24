# SETUP PROJECT ----

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  digits = 2,
  #   repos = c(CRAN = "https://cran.r-project.org/"),
  vsc.use_httpgd = TRUE,
  vsc.str.max.level = 2,
  vsc.viewer = TRUE
)

# SETUP RENV ----
if (!require("renv", quietly = TRUE)) {
  install.packages("renv")
  install.packages("park")
  print("Iniciando o RENV")
  renv::init()
} else {
  print("Ativando o RENV", call. = FALSE)
  renv::activate()
}

# SETUP DIRETÓRIOS ----
DIR_PROJECT <- getwd()
# [1] "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj"

DIR_PACKAGES <- .libPaths(renv::paths$library())


# SETUP PACKAGES ----
PACKAGES <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "readxl",
  "fs",
  "janitor",
  "writexl",
  "purrr",
  "forcats",
  "tibble",
  "lubridate",
  "tidyr",
  "stringr",
  "vroom",
  "utils",
  "usethis",
  "languageserver",
  "gt",
  "webshot2",
  "gtsummary"
)

# Instala e carrega os pacotes usando park (mais eficiente) ou renv
for (pkg in PACKAGES) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (requireNamespace("park", quietly = TRUE)) {
      park::park(pkg)
    } else {
      install.packages(pkg)
    }
    renv::snapshot(prompt = TRUE)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE, warn.conflicts = FALSE)
  print(paste("Pacote carregado:", pkg))
}

# REGAR PACOTES ----
#> Problemas com a função use()
# use("ggplot2", c("ggplot", "geom_point", "geom_line"))
# use("dplyr", c("mutate", "case_when", "filter", "select", "full_join", "right_join", "mutate_all", "rename_with"))
# use("janitor", c("clean_names"))
# use("writexl", c("write_xlsx"))
# use("base", c("set.seed", "sample", "nrow"))
# use("tidyverse", c("read_delim", "mutate", "across", "str_replace_all", "case_when", "filter", "arrange"))
# use("utils", c("write.table"))
# use("readxl", c("read_xlsx"))
# use("stringr", c("str_replace_all", "str_remove", "str_detect"))
# use("fs", c("dir_create", "file_copy", "file_move", "file_delete"))
# use("purrr", c("map", "map_df", "map_chr"))
# use("lubridate", c("ymd", "dmy", "mdy", "year", "month", "day"))
# use("vroom", c("vroom"))
