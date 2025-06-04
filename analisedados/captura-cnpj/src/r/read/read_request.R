# install.packages("tidyverse")

library(tidyverse)

base_request <- read_delim(
  file = "C:/Users/cesar.oliveira/github/projetos/analisedados/captura-cnpj/data/processed/cnpjs_data.csv",
  col_types = list(col_character()),
  delim = ";"
) |>
janitor::clean_names()
