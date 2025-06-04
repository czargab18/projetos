# MELHORAR ESTRUTURA E DESENVOLVER ALGUMAS FUNÇÕES
# FUNÇÕES:
#   1. read_cnpj: ler a base e importar os cnpj, sem repetições de cnpj
#   2. cnpj_save: verificar se o cjp já está salvo no arquivo desejado.
#   3. save_lista: salvar dados de cnpjs
#   3. request_cnpj: salvar dados da API do cnpj

import pandas
import os
import requests
import json
import time


def read_cnpj(caminho: str = None, namecol: str = None, repetir: bool = False):
    namecol = namecol.lower().strip()
    base = pandas.read_excel(caminho, dtype=str)
    for col in base.columns:
        if col.strip().lower() == namecol:
          # TODOS OS CNPJS TEM ZEROS A ESQUERDA
            cnpjs = base[col].astype(str).str.zfill(14).tolist()
            return cnpjs if repetir else list(set(cnpjs))
    raise ValueError(f"Coluna '{namecol}' não encontrada no arquivo.")


def is_save(cnpj: str, dados_existentes: dict):
    return cnpj in dados_existentes


def request_cnpj(url, cnpj, retries=3, delay=5):
    cnpj_nao_encontrado = []
    url_completa = url + cnpj

    for attempt in range(1, retries + 1):
        try:
            response = requests.get(url_completa, timeout=10)
            if response.status_code == 200:
                dados = response.json()
                return {cnpj: dados}
            else:
                print(f"Tentativa {attempt}/{retries}: {cnpj} não encontrado ou erro na requisição.")
        except requests.exceptions.RequestException as e:
            print(f"Tentativa {attempt}/{retries} falhou: {e}")
        time.sleep(delay)

    cnpj_nao_encontrado.append(cnpj)
    return {"nao_encontrados": cnpj_nao_encontrado}


def save_lista(save_in: str = None, dados: dict = None, namefile="cnpjs_request"):
    if not save_in or not dados:
        raise ValueError("Os parâmetros 'save_in' e 'dados' são obrigatórios.")

    os.makedirs(save_in, exist_ok=True)
    path = os.path.join(save_in, f"{namefile}.json")

    if os.path.exists(path):
        with open(path, "r", encoding="utf-8") as arquivo:
            try:
                dados_existentes = json.load(arquivo)
                if not isinstance(dados_existentes, dict):
                    dados_existentes = {}
            except json.JSONDecodeError:
                dados_existentes = {}
    else:
        dados_existentes = {}

    dados_existentes.update(dados)

    with open(path, "w", encoding="utf-8") as arquivo:
        json.dump(dados_existentes, arquivo, ensure_ascii=False, indent=4)
    print(f"Dados salvos em {path}.")


def load_existing_data(filepath: str):
    if os.path.exists(filepath):
        with open(filepath, "r", encoding="utf-8") as arquivo:
            try:
                return json.load(arquivo)
            except json.JSONDecodeError:
                return {}
    return {}


def cornullfalse(json_data):
    """
    Corrige os valores 'null' e 'false' em um JSON, substituindo-os por 'None' e 'False', respectivamente.

    Args:
        json_data (str ou dict): String JSON ou dicionário contendo valores 'null' ou 'false'.

    Returns:
        dict: Dicionário Python com os valores corrigidos.
    """
    if isinstance(json_data, str):
        json_data = json.loads(json_data)
    if isinstance(json_data, dict):
        json_str = json.dumps(json_data)
        json_str = json_str.replace("null", "None").replace("false", "False")
        return json.loads(json_str)
    raise ValueError(
        "O parâmetro 'json_data' deve ser uma string JSON ou um dicionário.")


if __name__ == "__main__":
    # LISTA DE CNPJs
    BASE_CAMINHO = "./data/processed/base_cnpjs.xlsx"
    COLUNA_CNPJ = "CNPJ Dispêndio"
    REPETIR_CNPJS = False

    try:
        LISTA_CNPJ = read_cnpj(caminho=BASE_CAMINHO, namecol=COLUNA_CNPJ, repetir=REPETIR_CNPJS)
        print(f"Total de CNPJs carregados: {len(LISTA_CNPJ)}")
    except Exception as e:
        print(f"Erro ao carregar a lista de CNPJs: {e}")
        LISTA_CNPJ = []

    # SETUP API
    URL = "https://minhareceita.org/"
    SAVE_DADOS_IN = "./data/processed/"
    NAMEFILE = "cnpjs_request"
    PATH_JSON = os.path.join(SAVE_DADOS_IN, f"{NAMEFILE}.json")

    # Carregar dados existentes
    try:
        cnpjs_salvos = load_existing_data(PATH_JSON)
        print(f"Total de CNPJs já salvos: {len(cnpjs_salvos)}")
    except Exception as e:
        print(f"Erro ao carregar dados existentes: {e}")
        cnpjs_salvos = {}

    # Verificar e salvar dados
    for cnpj in LISTA_CNPJ:
        if not is_save(cnpj, cnpjs_salvos):
            print(f"Requisitando dados para o CNPJ: {cnpj}")
            try:
                dados = request_cnpj(URL, cnpj)
                if isinstance(dados, dict):
                    # Corrigir valores 'null' e 'false' nos dados retornados
                    dados_corrigidos = cornullfalse(dados)
                    cnpjs_salvos.update(dados_corrigidos)
                else:
                    print(f"Erro ao requisitar dados para o CNPJ: {cnpj}")
            except Exception as e:
                print(f"Erro durante a requisição para o CNPJ {cnpj}: {e}")
        else:
            print(f"CNPJ {cnpj} já está salvo. Ignorando...")

    # Salvar os dados coletados
    try:
        save_lista(save_in=SAVE_DADOS_IN, dados=cnpjs_salvos, namefile=NAMEFILE)
        print("Processo concluído.")
    except Exception as e:
        print(f"Erro ao salvar os dados: {e}")
