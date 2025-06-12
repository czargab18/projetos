# source("config/setup.R")

# Carregar pacotes necessários
library(dplyr)
library(stringi)

# Função para gerar CNPJs aleatórios
gerar_cnpj <- function(n) {
  replicate(n, paste0(sample(0:9, 14, replace = TRUE), collapse = ""))
}

# Função para gerar nomes aleatórios
gerar_nome <- function(n) {
  replicate(n, stri_rand_strings(1, sample(5:10, 1), pattern = "[A-Za-z]"))
}

# Função para gerar IDs de projetos aleatórios
gerar_id_projeto <- function(n) {
  replicate(n, paste0("PROJ_", sample(1000:9999, 1)))
}

# Função para gerar datas aleatórias
gerar_data <- function(n, anos) {
  as.Date(sample(seq(Sys.Date() - anos * 365, Sys.Date(), by = "day"), n))
}

# Gerar o data.frame fictício
dados_ficticios <- data.frame(
  cnpj = gerar_cnpj(14),
  nome = gerar_nome(14),
  projeto = "gerar_id_projeto(14)",
  data_contratacao = gerar_data(14, anos = 1),
  data_encerramento = gerar_data(14, anos = 2)
)

# Exibir os dados gerados
print(dados_ficticios)