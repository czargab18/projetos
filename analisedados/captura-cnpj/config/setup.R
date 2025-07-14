# SETUP PROJECT ----
options(
  stringsAsFactors = FALSE,
  repos = c(CRAN = "https://vps.fmvz.usp.br/CRAN/")
)

# SETUP RENV ----
if (!require("renv", quietly = TRUE)) {
  install.packages("renv")
} else {
  print("iniciando o RENV")
  renv::init()
  #> 3: Activate the project
}
# ativar caso já exista
renv::activate()


# SETUP DIRETÓRIOS ----
DIR_PROJECT <- getwd()
# [1] "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj"

DIR_PACKAGES <- .libPaths(renv::paths$library())


# SETUP PACKAGES ----
PACKAGES <- c("dplyr", "ggplot2", "readr", "readxl", "fs", "janitor", "writexl", "purrr", "tidyverse")

# Instala e carrega os pacotes usando {renv}
for (pkg in PACKAGES) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    renv::snapshot(prompt = TRUE)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
  print(paste("Pacote carregado:", pkg))
}

# SETUP USE() ----
use <- function(pkg, funs = NULL) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  if (!is.null(funs)) {
    for (f in funs) {
      assign(f, get(f, envir = asNamespace(pkg)), envir = .GlobalEnv)
    }
  }
}

# use() encontrados nos scripts R

# REGAR PACOTES ----
use("ggplot2", c("ggplot", "geom_point", "geom_line"))
use("dplyr", c("mutate", "case_when", "filter", "select", "full_join", "right_join", "mutate_all", "rename_with"))
use("janitor", c("clean_names"))
use("writexl", c("write_xlsx"))
use("base", c("set.seed", "sample", "nrow"))
use("tidyverse", c("read_delim", "mutate", "across", "str_replace_all", "case_when", "filter", "arrange"))
use("utils", c("write.table"))
use("readxl", c("read_xlsx"))
use("stringr", c("str_replace_all", "str_remove", "str_detect"))
