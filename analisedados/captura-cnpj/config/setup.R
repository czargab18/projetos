# SETUP RENV ----
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::activate()

# SETUP PROJECT ----
options(
  stringsAsFactors = FALSE,
  repos = c(CRAN = "https://vps.fmvz.usp.br/CRAN/")
)

# SETUP DIRETÓRIOS ----
REMOTE_DIR <- "C:/Users/cesar.oliveira/github/portifolio/projetos/analisedados/captura-cnpj"
# LOCAL_DIR <- "C:/Users/cesargabriel/github/portifolio/projetos/analisedados/captura-cnpj"

setwd(REMOTE_DIR)
getwd()

# SETUP PACKAGES ----
PACKAGES <- c("dplyr", "ggplot2", "readr", "readxl", "fs", "janitor", "writexl", "purrr")

# Instala e carrega os pacotes usando {renv}
for (pkg in PACKAGES) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    renv::snapshot(prompt = FALSE) # Atualiza o renv.lock automaticamente
  }
  library(pkg, character.only = TRUE)
}
