################################################################################
## exemplo:
# > full_join(data1, data2, by = "ID")
################################################################################
## juntarpor uma coluna que nas duas bases tem nomes diferentes
# > dplyr::full_join(x = BASE, y = base, by = c("coluna_x" = "coluna_y"))
################################################################################
# referencias:
# - https://www.youtube.com/watch?v=xnUo25VRH70
# - https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti
################################################################################

# SET UP PACKAGES ----
use("dplyr", c("full_join"))

library(dplyr)

# CARREGAR BASES ----
source("src/r/read/read_base_mcti.R")
source("src/r/read/read_request.R")

# JUNTAR BASES ----
base_mcti_request <- dplyr::right_join(
  x = base_mcti,
  y = base_request,
  by = c("cnpj_dispendio" = "cnpj")
) |>
  dplyr::mutate_all(as.character)

# View(base_mcti_request[23:26, ])

# SALVANDO BASE COMPLETA ----
# usar separador ";"
# write.table(
#   x = base_mcti_request,
#   file = "data/processed/base_mcti_request.csv",
#   sep = ";",
#   row.names = FALSE,
#   col.names = TRUE,
#   quote = TRUE,
#   fileEncoding = "UTF-8"
# )