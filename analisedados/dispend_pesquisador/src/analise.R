use("readxl", "read_xlsx")

dados_gerados <-
  readxl::read_xlsx(
    path = "./data/dados_gerados.xlsx"
  ) |>
  dplyr::select(
    cnpj,
    pesquisador,
    meses,
    incentivo
  ) |>
  dplyr::group_by(cnpj) |>
  dplyr::summarise(
    qtd_pesq = dplyr::n_distinct(pesquisador),
    qtd_meses = sum(meses, na.rm = TRUE),
    media_pesq = mean(qtd_pesq, na.rm = TRUE),
    media_meses = mean(meses, na.rm = TRUE)
  )

#mean_qtd_pesq_empresas <-
mean(dados_gerados$qtd_pesq)
#mean_qtd_meses_empresas <-
mean(dados_gerados$qtd_meses)

# print(mean_qtd_pesq_empresas)
# print(mean_qtd_meses_empresas)

# Problema:
# avaliar consideração de quantidades de pesquisadores com média de pesquisadores para avaliação de projetos.

# comentário:
# Primeiro, não existe média de pessoas. Como cada individuo conta apenas 1 vez, média de individuos mean = (1/n) sum(n*1) = 1.
# O que pode existir é:
# Para cada empresa, calcula-se o número de pesquisadores e depois obtém-se a média dessa quantidade entre todas as empresas.

# Considerando a soma detodos os meses trabalhados pelos pesquisadores que estariam válidos para a empresa.

# A tibble: 540 × 5
#    cnpj           qtd_pesq qtd_meses media_pesq media_meses
#    <chr>             <int>     <dbl>      <dbl>       <dbl>
#  1 00010819062423        1         3          1         3
#  2 00149523432229        1        10          1        10
#  3 00395021482709        2        17          2         8.5
#  4 00472076147153        1         8          1         8
#  5 00878107952321        2        34          2        17
#  6 00975557772723        2        15          2         7.5
#  7 01001146832025        1         5          1         5
#  8 01112632397185        1        11          1        11
#  9 01209901671017        1         0          1         0
# 10 01285403696894        1         2          1         2
# # ℹ 530 more rows
# # ℹ Use `print(n = ...)` to see more rows
