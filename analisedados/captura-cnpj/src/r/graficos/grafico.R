# CONFIGURAÇÕES INICIAIS ----
#> carregar e unir as bases de dados
source("src/r/read/read_and_join_bases.R")
#> carregar funções auxiliares para gráficos
source("src/r/graficos/temas.R")

# GRÁFICO: TIPOS DE PARCERIAS ----
plot_tipo_dispendio <-
  base_mcti_request |>
  dplyr::select(tipo_dispendio, cnpj_dispendio) |>
  dplyr::group_by(tipo_dispendio) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = purrr::map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    tipo_dispendio = factor(
      tipo_dispendio,
      levels = tipo_dispendio[order(quantidade)]
    )
  ) |>
  dplyr::select(tipo_dispendio, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = tipo_dispendio, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Tipos de Parcerias",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(
    aes(label = quantidade),
    color = "#000000",
    size = 4,
    hjust = 1
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_tipo_dispendio,
  filename = "resultado/graficos/tipos_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)

# GRÁFICO:Unidade federativas das ICTs ----
plot_uf_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in% c("Instituição de Pesquisa", "Universidades")
  ) |>
  dplyr::select(uf.parceira, cnpj_dispendio) |>
  dplyr::group_by(uf.parceira) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = purrr::map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    uf.parceira = factor(
      uf.parceira,
      levels = uf.parceira[order(quantidade)]
    )
  ) |>
  dplyr::select(uf.parceira, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = uf.parceira, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "União Federativa das ICTS em projetos de P,D&I",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(
    aes(label = quantidade),
    color = "#000000",
    size = 4,
    hjust = -0.05
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_uf_dispendio,
  filename = "resultado/graficos/uf_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300
)

# GRÁFICO: REGIÃO das ICTs ----
plot_regiao_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in%
      c("Instituição de Pesquisa", "Universidades") &
      regiao_dispendio != "EXTERIOR"
  ) |>
  dplyr::select(regiao_dispendio, cnpj_dispendio) |>
  dplyr::group_by(regiao_dispendio) |>
  dplyr::summarise(cnpj_dispendio = list(cnpj_dispendio)) |>
  dplyr::mutate(
    cnpj_dispendio = purrr::map(cnpj_dispendio, unique),
    quantidade = lengths(cnpj_dispendio),
    regiao_dispendio = factor(
      regiao_dispendio,
      levels = regiao_dispendio[order(quantidade)]
    )
  ) |>
  dplyr::select(regiao_dispendio, quantidade) |>
  dplyr::arrange(desc(quantidade)) |>
  ggplot(aes(x = regiao_dispendio, y = quantidade)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Região das ICTs em projetos de P,D&I",
    y = "Quantidade de Projetos"
  ) +
  ggplot2::geom_text(
    aes(label = quantidade),
    color = "#ffffff",
    size = 4,
    hjust = 1.2
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_regiao_dispendio,
  filename = "resultado/graficos/regiao_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)

# GRÁFICO: valor total por região ----
plot_tipo_dispendio <-
  base_mcti_request |>
  dplyr::select(tipo_dispendio, valor) |>
  dplyr::group_by(tipo_dispendio) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE)) |>
  dplyr::mutate(
    tipo_dispendio = factor(
      tipo_dispendio,
      levels = tipo_dispendio[order(valor)]
    )
  ) |>
  ggplot2::ggplot(aes(x = tipo_dispendio, y = valor)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_discrete(
    limits = rev(levels(base_mcti_request$tipo_dispendio))
  ) +
  ggplot2::labs(
    title = "Região das ICTs e Universidades em projetos de P,D&I",
    y = "Valor Total (em milhões de reais)"
  ) +
  ggplot2::geom_text(
    aes(label = scales::comma(valor)),
    color = "#ffffff",
    size = 4,
    hjust = 1.1
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_tipo_dispendio,
  filename = "resultado/graficos/valor_tipoos_dispendio.png",
  width = 10,
  height = 6,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)

# GRÁFICO: VALOR TOTAL POR REGIÃO ----

plot_valor_regiao_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in%
      c("Instituição de Pesquisa", "Universidades") &
      regiao_dispendio != "EXTERIOR"
  ) |>
  dplyr::select(regiao_dispendio, valor) |>
  dplyr::group_by(regiao_dispendio) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE)) |>
  dplyr::mutate(
    regiao_dispendio = factor(
      regiao_dispendio,
      levels = regiao_dispendio[order(valor)]
    )
  ) |>
  ggplot2::ggplot(aes(x = regiao_dispendio, y = valor)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_discrete(
    limits = rev(levels(base_mcti_request$regiao_dispendio))
  ) +
  ggplot2::labs(
    title = "Região das ICTs e Universidades em projetos de P,D&I",
    y = "Valor Total (em milhões de reais)"
  ) +
  ggplot2::geom_text(
    aes(label = scales::comma(valor)),
    color = "#000000",
    size = 4,
    hjust = 1.1
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_valor_regiao_dispendio,
  filename = "resultado/graficos/valor_regiao_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300
)

# GRÁFICO: VALOR TOTAL POR UF ----
plot_valor_uf_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in%
      c("Instituição de Pesquisa", "Universidades") &
      regiao_dispendio != "EXTERIOR"
  ) |>
  dplyr::select(uf.parceira, valor) |>
  dplyr::group_by(uf.parceira) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE)) |>
  dplyr::mutate(
    uf.parceira = factor(
      uf.parceira,
      levels = uf.parceira[order(valor)]
    )
  ) |>
  ggplot2::ggplot(aes(x = uf.parceira, y = valor)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_discrete(
    limits = rev(levels(base_mcti_request$uf.parceira))
  ) +
  ggplot2::labs(
    title = "União Federativa das ICTS em projetos de P,D&I",
    y = "Valor Total (em milhões de reais)"
  ) +
  ggplot2::geom_text(
    aes(label = scales::comma(valor)),
    color = "#000000",
    size = 4,
    hjust = 0.5
  ) +
  tema()

ggplot2::ggsave(
  plot = plot_valor_uf_dispendio,
  filename = "resultado/graficos/valor_uf_parcerias.png",
  width = 10,
  height = 6,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)

# TABELAS TOP 10 ----
table_top10_uf_dispendio <-
  base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in%
      c("Instituição de Pesquisa", "Universidades") &
      uf_comp != "EXTERIOR"
  ) |>
  dplyr::select(uf_comp, valor) |>
  dplyr::group_by(uf_comp) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE)) |>
  dplyr::mutate(
    uf_comp = factor(
      uf_comp,
      levels = uf_comp[order(valor)]
    )
  ) |>
  dplyr::rename(
    "UF Parceira" = uf_comp,
    "Valor Total (R$)" = valor
  ) |>
  dplyr::arrange(desc(`Valor Total (R$)`)) |>
  dplyr::slice_head(n = 10) |>
  gt::gt() |>
  gt::tab_header(
    title = "Investimento em P,D&I por União Federativa"
  ) |>
  gt::cols_label(
    "UF Parceira" = "UF Parceira",
    "Valor Total (R$)" = "Valor Total (R$)"
  ) |>
  gt::fmt_currency(
    columns = "Valor Total (R$)",
    currency = "BRL"
  ) |>
  gt::cols_align(
    align = "left",
    columns = c("UF Parceira", "Valor Total (R$)")
  )

gt::gtsave(
  table_top10_uf_dispendio,
  filename = "resultado/tabelas/top10_uf_dispendio.png"
)

# Tabela 10 Universidade ----
table_top10_tipodispendio <-
  base_mcti_request |>
  dplyr::select(tipo_dispendio, valor) |>
  dplyr::group_by(tipo_dispendio) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE)) |>
  dplyr::mutate(
    tipo_dispendio = factor(
      tipo_dispendio,
      levels = tipo_dispendio[order(valor)]
    )
  ) |>
  dplyr::rename(
    "UF Parceira" = tipo_dispendio,
    "Valor Total (R$)" = valor
  ) |>
  dplyr::arrange(desc(`Valor Total (R$)`)) |>
  gt::gt() |>
  gt::tab_header(
    title = "Investimento em P,D&I por Universidade"
  ) |>
  gt::cols_label(
    "UF Parceira" = "UF Parceira",
    "Valor Total (R$)" = "Valor Total (R$)"
  ) |>
  gt::fmt_currency(
    columns = "Valor Total (R$)",
    currency = "BRL"
  ) |>
  gt::cols_align(
    align = "left",
    columns = c("UF Parceira", "Valor Total (R$)")
  ) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(
      columns = "UF Parceira",
      rows = `UF Parceira` %in% c("Instituição de Pesquisa", "Universidades")
    )
  )

gt::gtsave(
  table_top10_tipodispendio,
  filename = "resultado/tabelas/top10_tipodispendio.png"
)

# TABELAS: top 10 universidade que mais aparecem em projetos de P,D&I ----

base_mcti_request |>
  dplyr::filter(
    tipo_dispendio %in%
      c("Instituição de Pesquisa", "Universidades") &
      tipo_dispendio != "EXTERIOR"
  ) |>
  dplyr::select(
    razao_social_dispendio,
    cnpj_dispendio
  ) |>
  dplyr::summarise(qtd_cnpjs_unicos = dplyr::n_distinct(cnpj_dispendio)) |>
  dplyr::arrange(desc(qtd_cnpjs_unicos)) |>
  dplyr::rename(
    "Tipo de Dispêndio" = tipo_dispendio,
    "Qtd. CNPJs Únicos" = qtd_cnpjs_unicos
  ) |>
  gt::gt() |>
  gt::tab_header(
    title = "Quantidade de CNPJs únicos por Tipo de Dispêndio"
  ) |>
  gt::cols_label(
    "Tipo de Dispêndio" = "Tipo de Dispêndio",
    "Qtd. CNPJs Únicos" = "Qtd. CNPJs Únicos"
  ) |>
  gt::cols_align(
    align = "left",
    columns = c("Tipo de Dispêndio")
  )

gt::gtsave(
  table_cnpjs_unicos_tipodispendio,
  filename = "resultado/tabelas/cnpjs_unicos_tipodispendio.png"
)

# TABELA: Top 10 Universidades ou Institutos Federais por quantidade de CNPJs únicos ----
# table_top10_universidades <-
base_mcti_request |>
  dplyr::filter(
    tipo_dispendio == "Universidades"
  ) |>
  dplyr::select(razao_social_dispendio, valor) |>
  dplyr::group_by(razao_social_dispendio) |>
  dplyr::summarise(
    investimento = sum(valor, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(investimento)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::rename(
    "Universidade" = razao_social_dispendio,
    "Investimento (R$)" = investimento
  ) |>
  gt::gt() |>
  gt::tab_header(
    title = "Universidades com mais Investimentos em P,D&I"
  ) |>
  gt::cols_label(
    "Universidade" = "Universidade",
    "Investimento (R$)" = "Investimento (R$)"
  ) |>
  gt::fmt_currency(
    columns = "Investimento (R$)",
    currency = "BRL"
  ) |>
  gt::cols_align(
    align = "left",
    columns = c("Universidade", "Investimento (R$)")
  )

gt::gtsave(
  table_top10_universidades,
  filename = "resultado/tabelas/top10_universidades_cnpjs.png"
)
