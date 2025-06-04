# DEFININDO PACOTES ESSENCIAIS DA LINGUAGEM R PARA ANALISE DE DADOS
PACKAGES <- c("tidyverse", "dplyr", "ggplot2", "readr", "fs", "janitor", "renv", "purrr") # removido vírgula duplicada

# instalando os pacotes
for (pgk in PACKAGES) {
  if (!require(pgk, character.only = TRUE, quietly = TRUE)) {
    install.packages(pgk, lib = LIBS_R)
    library(pgk, character.only = TRUE, lib.loc = LIBS_R)
  } else {
    library(pgk, character.only = TRUE, lib.loc = LIBS_R)
  }
}






criar_dir <- function(caminho) {
  criar<- function(x) {
    if (!fs::dir_exists(caminho)) {
      fs::dir_create(x, recurse = TRUE)
      message("Diretório criado: ", caminho)
  } else {
      message("Diretório já existe: ", caminho)
  }
  }
  # Verifica se o pacote fs está instalado, se não estiver, instala e carrega
  if (!require(fs)) {
    install.packages("fs")
    library(fs)
    # Verifica se o diretório existe, se não existir, cria
    criar(caminho)
  } else {
    library(fs)
    # Verifica se o diretório existe, se não existir, cria
    criar(caminho)
  }
}

criar_dir(LIBS_R)