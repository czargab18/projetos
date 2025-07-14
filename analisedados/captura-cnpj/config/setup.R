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
