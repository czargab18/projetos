import json
import pandas as pd

# Carrega o arquivo JSON
with open("./data/processed/cnpjs_request.json", "r", encoding="utf-8") as file:
    data = json.load(file)

# Cria uma lista para armazenar os dados formatados
rows = []

# Itera sobre os CNPJs e extrai as informações relevantes
for cnpj, info in data.items():
    rows.append({
        "CNPJ": cnpj,
        "Identificador Matriz/Filial": info.get("identificador_matriz_filial"),
        "Descrição Matriz/Filial": info.get("descricao_identificador_matriz_filial"),
        "Nome Fantasia": info.get("nome_fantasia"),
        "Situação Cadastral": info.get("situacao_cadastral"),
        "Descrição Situação Cadastral": info.get("descricao_situacao_cadastral"),
        "Data Situação Cadastral": info.get("data_situacao_cadastral"),
        "Motivo Situação Cadastral": info.get("motivo_situacao_cadastral"),
        "Descrição Motivo Situação Cadastral": info.get("descricao_motivo_situacao_cadastral"),
        "Nome Cidade no Exterior": info.get("nome_cidade_no_exterior"),
        "Código País": info.get("codigo_pais"),
        "País": info.get("pais"),
        "Data Início Atividade": info.get("data_inicio_atividade"),
        "CNAE Fiscal": info.get("cnae_fiscal"),
        "Descrição CNAE Fiscal": info.get("cnae_fiscal_descricao"),
        "Descrição Tipo de Logradouro": info.get("descricao_tipo_de_logradouro"),
        "Logradouro": info.get("logradouro"),
        "Número": info.get("numero"),
        "Complemento": info.get("complemento"),
        "Bairro": info.get("bairro"),
        "CEP": info.get("cep"),
        "UF": info.get("uf"),
        "Código Município": info.get("codigo_municipio"),
        "Código Município IBGE": info.get("codigo_municipio_ibge"),
        "Município": info.get("municipio"),
        "DDD Telefone 1": info.get("ddd_telefone_1"),
        "DDD Telefone 2": info.get("ddd_telefone_2"),
        "DDD Fax": info.get("ddd_fax"),
        "Situação Especial": info.get("situacao_especial"),
        "Data Situação Especial": info.get("data_situacao_especial"),
        "Opção pelo Simples": info.get("opcao_pelo_simples"),
        "Data Opção pelo Simples": info.get("data_opcao_pelo_simples"),
        "Data Exclusão do Simples": info.get("data_exclusao_do_simples"),
        "Opção pelo MEI": info.get("opcao_pelo_mei"),
        "Data Opção pelo MEI": info.get("data_opcao_pelo_mei"),
        "Data Exclusão do MEI": info.get("data_exclusao_do_mei"),
        "Razão Social": info.get("razao_social"),
        "Código Natureza Jurídica": info.get("codigo_natureza_juridica"),
        "Natureza Jurídica": info.get("natureza_juridica"),
        "Qualificação do Responsável": info.get("qualificacao_do_responsavel"),
        "Capital Social": info.get("capital_social"),
        "Código Porte": info.get("codigo_porte"),
        "Porte": info.get("porte"),
        "Ente Federativo Responsável": info.get("ente_federativo_responsavel"),
        "Regime Tributário": info.get("regime_tributario"),
        # Lista de listas para QSA
        "QSA": [
            [
                socio.get("identificador_de_socio"),
                socio.get("nome_socio"),
                socio.get("cnpj_cpf_do_socio"),
                socio.get("codigo_qualificacao_socio"),
                socio.get("qualificacao_socio"),
                socio.get("data_entrada_sociedade"),
                socio.get("codigo_pais"),
                socio.get("pais"),
                socio.get("cpf_representante_legal"),
                socio.get("nome_representante_legal"),
                socio.get("codigo_qualificacao_representante_legal"),
                socio.get("qualificacao_representante_legal"),
                socio.get("codigo_faixa_etaria"),
                socio.get("faixa_etaria")
            ]
            for socio in (info.get("qsa") or [])  # Garante que seja uma lista ou uma lista vazia
        ],
        # Lista de listas para CNAEs Secundários
        "CNAEs Secundários": [
            [
                cnae.get("codigo"),
                cnae.get("descricao")
            ]
            for cnae in info.get("cnaes_secundarios", [])
        ]
    })

# Cria o DataFrame
df = pd.DataFrame(rows)

# Exibe o DataFrame
print(df)

# Salva o DataFrame em um arquivo CSV (opcional)
df.to_csv("./data/processed/cnpjs_data.csv", index=False, encoding="utf-8", sep=";")