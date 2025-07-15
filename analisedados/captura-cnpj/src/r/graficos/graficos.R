
uf_dispendio <-
base_mcti_request |>
  dplyr::count(uf.y) |>
  dplyr::mutate(uf.y = factor(uf.y, levels = uf.y[order(n)])) |>
  ggplot(aes(x = uf.y, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

theme_custom(
  plot_tipo,
  title = "União Federativa dass parcerias em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)


regiao_dispendio <-
base_mcti_request |>
  dplyr::count(regiao_dispendio) |>
  dplyr::mutate(regiao_dispendio = factor(regiao_dispendio, levels = regiao_dispendio[order(n)])) |>
  ggplot(aes(x = regiao_dispendio, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

theme_custom(
  regiao_dispendio,
  title = "Regiao dos parceiros em projetos de P,D&I",
  title.y = "Quantidade de Projetos"
)

# PRINCIPAIS FUNDAÇÕES  ----
# tipo de fundação publica
base_mcti_request |>
  dplyr::select(natureza_juridica, razao_social.y) |>
  dplyr::filter(
    stringr::str_detect(natureza_juridica, "Fundação Pública")
  ) |>
  dplyr::count(razao_social.y) |>
  dplyr::arrange(desc(n)) |>
  View()


# nomes da fundação (publica ou privada)
base_mcti_request |>
  dplyr::select(razao_social.y) |>
  dplyr::filter(
    stringr::str_detect(razao_social.y, "FUNDACAO (UNIVERSIDADE|FACULDADE)")
  ) |>
  dplyr::count(razao_social.y) |>
  dplyr::arrange(desc(n))  |>
  View()





