source("config/setup.R")

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
  replicate(n, paste0("PROJ_", sample(1000:9999, 1, replace = TRUE)))
}

# Função para gerar datas aleatórias
gerar_data <- function(n, anos) {
  as.Date(sample(seq(Sys.Date() - anos * 365, Sys.Date(), by = "day"), n))
}

# Gerar o data.frame fictício com muitos registros
n_registros <- 10000

dados_ficticios <- data.frame(
  cnpj = gerar_cnpj(n_registros),
  nome = gerar_nome(n_registros),
  projeto = gerar_id_projeto(n_registros),
  data_contratacao = gerar_data(n_registros, anos = 1),
  data_encerramento = gerar_data(n_registros, anos = 2)
)

# Exibir os primeiros registros gerados
print(head(dados_ficticios))