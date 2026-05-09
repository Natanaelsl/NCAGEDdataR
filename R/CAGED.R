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

  # --- PRIORIDADE 1: ACESSO DIRETO (SEMÂNTICA LOCAL) ---
  # Se o usuário passou um arquivo específico, ele quer pular todo o pipeline.
  if (!is.null(arquivo_alvo)) {
    if (!file.exists(arquivo_alvo)) {
      cli::cli_alert_danger("Arquivo alvo não encontrado: {.file {arquivo_alvo}}")
      stop("Erro de caminho local.")
    }
    cli::cli_alert_info("Modo Manual: Abrindo {.file {basename(arquivo_alvo)}}")
    return(arrow::open_dataset(arquivo_alvo))
  }

  # --- PRIORIDADE 2: CACHE INTELIGENTE (SEMÂNTICA DE PERFORMANCE) ---
  # Se ref for NULL, buscamos o período mais novo no Drive para comparar com o local.
  periodo_remoto <- if(is.null(ref)) download_caged(temp = TRUE) else ref

  # Busca o cache local para comparação
  arquivo_local <- list.files(destino, pattern = "CAGED_CONSOLIDADO_.*\\.parquet$", full.names = TRUE) %>%
    sort(decreasing = TRUE) %>% .[1]
  periodo_local <- stringr::str_extract(basename(arquivo_local), "\\d{4,6}")

  if (!forcar && !is.na(arquivo_local) && !is.na(periodo_remoto) && periodo_local >= periodo_remoto) {
    cli::cli_alert_success("Cache atualizado ({periodo_local}). Operação instantânea.")
    return(arrow::open_dataset(arquivo_local))
  }

  # --- PRIORIDADE 3: ATUALIZAÇÃO (SEMÂNTICA DE FLUXO) ---
  # Se chegou aqui, é porque o cache está velho, não existe ou o usuário forçou.
  cli::cli_h1("🚀 Pipeline de Atualização")
  # ... segue o download_caged(ref = periodo_remoto) e processar_caged() ...
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
