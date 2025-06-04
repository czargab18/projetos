# Captura de CNPJs

Este projeto realiza a análise e manipulação de dados relacionados a CNPJs, utilizando R e Python para leitura, transformação e exportação de dados.

## Estrutura do Projeto

Abaixo está a estrutura principal do projeto e a descrição de cada pasta:

```
.
├── config/          # Configurações e scripts de setup do projeto
├── data/            # Dados utilizados no Projeto
│   ├── processed/   # Dados processados
│   └── raw/         # Dados brutos
├── renv/            # Configuração do ambiente R gerenciado pelo renv
├── src/             # Scripts principais do projeto
│   ├── python/      # Scripts Python para manipulação de dados
│   └── r/           # Scripts R para leitura, manipulação e exportação de dados
```

## Configuração do Ambiente

1. Certifique-se de ter o R instalado.
2. Instale o pacote `renv` caso ainda não esteja disponível:
   ```r
   install.packages("renv")
   ```
3. Ative o ambiente `renv`:
   ```r
   renv::activate()
   ```
4. Instale os pacotes necessários:
   ```r
   renv::restore()
   ```

## Scripts Principais

### R Scripts

- **Leitura de Dados**
  - [`read_base_mcti.R`](src/r/read/read_base_mcti.R): Lê a base de dados `base_cnpjs.xlsx`.
  - [`read_request.R`](src/r/read/read_request.R): Lê os dados de requisição.
  - [`sample_base.R`](src/r/read/sample_base.R): Gera uma amostra da base de dados.

- **Manipulação de Dados**
  - [`join_base_and_request.R`](src/r/manipul/join_base_and_request.R): Realiza a junção entre bases.

- **Exportação de Dados**
  - [`export_base.R`](src/r/save/export_base.R): Exporta os dados processados para um arquivo Excel.

### Python Scripts

- [`request.py`](src/python/request.py): Realiza requisições para APIs.
- [`transform_json_csv.py`](src/python/transform_json_csv.py): Transforma dados JSON em CSV.

## Dados

- **Brutos**: Localizados em `data/raw/`.
- **Processados**: Localizados em `data/processed/`.

## Contribuição

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou enviar pull requests.

## Licença

Este projeto está sob a licença MIT.