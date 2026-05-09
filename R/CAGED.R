#' Orquestrador Principal: Extração e Tratamento do Novo CAGED
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' A função `CAGED()` é a solução 'One-Click' do pacote NCAGEDdataR. Ela automatiza o fluxo
#' completo: localiza a última competência no Google Drive, baixa os arquivos originais
#' para uma pasta segura, padroniza a estrutura das planilhas e persiste os dados
#' em formato colunar de alta performance (Apache Parquet).
#'
#' @param ref É a referência temporal (ex: "202401"). Serve para o download. Se for NULL, o pacote descobre sozinho qual é o mês mais novo no Drive.
#' @param destino Caminho da pasta onde os arquivos Parquet serão salvos.
#' @param arquivo_alvo É o caminho físico para um arquivo .parquet. Se preenchido, o pacote ignora a internet e abre o arquivo diretamente (Modo Manual).
#' @param parquet_individual Lógico. Se TRUE, salva cada aba original como um arquivo Parquet separado. Se FALSE (padrão), consolida tudo em um único dataset.
#' @param forcar Um booleano (TRUE/FALSE). Se TRUE, ignora o cache e refaz o download/processamento. Essencial para corrigir dados corrompidos ou capturar retificações oficiais do Ministério do Trabalho.
#'
#' @return Retorna uma conexão de alta performance (Dataset Arrow) apontando para os dados consolidados, pronta para avaliação preguiçosa (Lazy Evaluation) via `dplyr`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fluxo automático (baixa a última competência e consolida)
#' base_fiscal <- CAGED()
#'
#' # Filtrando dados de Goiás usando Arrow
#' library(dplyr)
#' dados_goias <- base_fiscal %>%
#'   filter(UF == "GO") %>%
#'   collect()
#' }
#'
# 3. FUNÇÃO ORQUESTRADORA (Conexão das Pontas) ----
CAGED <- function(ref = NULL,
                  destino = "dados_caged_parquet",
                  arquivo_alvo = NULL,
                  parquet_individual = FALSE,
                  forcar = FALSE) {

  # --- 1. PRIORIDADE MÁXIMA: ARQUIVO ESPECÍFICO ---
  # Se o usuário já deu o caminho (arquivo_alvo), abre direto e ignora o resto.
  if (!is.null(arquivo_alvo)) {
    if (!file.exists(arquivo_alvo)) {
      cli::cli_alert_danger("Erro: Arquivo {.file {arquivo_alvo}} não encontrado.")
      stop("Caminho inválido.", call. = FALSE)
    }
    return(arrow::open_dataset(arquivo_alvo))
  }

  # --- 2. VERIFICAÇÃO DE CACHE (SMART CACHE) ---
  # Busca o consolidado mais recente para comparação de datas
  arquivo_local <- list.files(destino, pattern = "CAGED_CONSOLIDADO_.*\\.parquet$", full.names = TRUE) %>%
    sort(decreasing = TRUE) %>%
    .[1]

  periodo_local <- stringr::str_extract(basename(arquivo_local), "\\d{4,6}")

  # Sensor: apenas verifica qual o mês mais novo na nuvem (sem baixar nada)
  periodo_remoto <- download_caged(temp = TRUE)

  # Se o local for >= remoto (ex: 202605 >= 202605), retornamos o que já temos
  if (!forcar && !is.na(arquivo_local) && !is.na(periodo_remoto) && periodo_local >= periodo_remoto) {
    cli::cli_alert_success("Dados locais atualizados ({periodo_local}). Pulando download.")

    # Aqui respeitamos o argumento parquet_individual mesmo no cache
    if (parquet_individual) {
      cli::cli_alert_info("Modo individual: Por favor, indique o arquivo em 'arquivo_alvo' ou processe os dados.")
    }
    return(arrow::open_dataset(arquivo_local))
  }

  # --- 3. FLUXO DE ATUALIZAÇÃO (Sincronização com 2026) ---
  cli::cli_h1("🚀 Atualização para versão: {periodo_remoto}")

  # Download real (temp = FALSE para salvar no disco)
  download_caged(ref = if(is.null(ref)) periodo_remoto else ref, temp = FALSE)

  # Processamento respeitando sua flag de parquets individuais
  processar_caged(destino = destino, parquet_individual = parquet_individual)

  # --- 4. RETORNO PÓS-PROCESSAMENTO ---
  if (parquet_individual) {
    cli::cli_alert_info("Processamento concluído. Parquets individuais gerados em {.path {destino}}.")
    # Como são múltiplos arquivos, retornamos o diretório para o Arrow mapear
    return(arrow::open_dataset(destino))
  } else {
    arquivos_consolidados <- list.files(destino, pattern = "CONSOLIDADO", full.names = TRUE)
    caminho_novo <- tail(sort(arquivos_consolidados), 1)
    cli::cli_alert_success("Dataset consolidado carregado.")
    return(arrow::open_dataset(caminho_novo))
  }
}

# Nova Arquitetura de Cache Inteligente (Smart Cache)
# A função CAGED() agora é "consciente". Ela não apenas baixa dados, mas gerencia o estado local para economizar tempo e banda.
#
# Semântica dos Argumentos
# Para eliminar redundâncias e tornar a API mais intuitiva, definimos as seguintes funções:
#
#   ref (O que você quer?): É a referência temporal (ex: "202401"). Serve para o download. Se for NULL, o pacote descobre sozinho qual é o mês mais novo no Drive.
#
# arquivo_alvo (Onde está?): É o caminho físico para um arquivo .parquet. Se preenchido, o pacote ignora a internet e abre o arquivo diretamente (Modo Manual).
#
# forcar (A válvula de escape): Um booleano (TRUE/FALSE). Se TRUE, ignora o cache e refaz o download/processamento. Essencial para corrigir dados corrompidos ou capturar retificações oficiais do Ministério do Trabalho.
#
# 3. Fluxo de Decisão da Função CAGED()
# A lógica interna segue este fluxo hierárquico:
#
#   Verificação de Arquivo Direto: Se arquivo_alvo existir, abre e retorna.
#
# Comparação de Metadados: Se não houver arquivo alvo, o pacote compara o período do arquivo local mais recente com o período disponível no Google Drive (via download_caged(temp = TRUE)).
#
# Decisão de Performance:
#
#   Se Local >= Remoto: Carrega o cache instantaneamente (via arrow).
#
# Se Remoto > Local ou forcar = TRUE: Inicia o download e reprocessamento completo.
