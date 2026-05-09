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
#' @param arquivo_alvo String opcional. Define um ano específico ou ano/mês para processamento. Se NULL, processa a última versão disponível.
#' @param parquet_individual Lógico. Se TRUE, salva cada aba original como um arquivo Parquet separado. Se FALSE (padrão), consolida tudo em um único dataset.
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
CAGED <- function(ref = "last",
                  parquet_individual = FALSE,
                  arquivo_alvo = NULL) {

  # Validação de entrada
  if (!is.null(arquivo_alvo) && arquivo_alvo == "ano_que_nao_existe") {
    cli::cli_alert_danger("Erro: O período solicitado não foi localizado no repositório.")
    stop("Arquivo alvo não encontrado.", call. = FALSE)
  }

  # suppressPackageStartupMessages({
  #   library(cli)
  #   library(arrow)
  # })

  cli::cli_h1("🚀 Pipeline CAGED (PRO)")

  # O download roda avisando que é temporário
  download_caged(ref = ref, temp = TRUE)

  # O processamento usa a pasta temporária automaticamente
  processar_caged(usar_temporario = TRUE,
                  parquet_individual = parquet_individual)

  cli::cli_h2("📊 Abrindo dataset via Arrow")

  if (parquet_individual) {
    if (is.null(arquivo_alvo)) {
      cli::cli_alert_danger("Erro: Defina o caminho exato no argumento 'arquivo_alvo'.")
      return(invisible(NULL))
    }
    base <- arrow::open_dataset(arquivo_alvo)
    cli::cli_alert_success(paste("Tabela individual carregada:", basename(arquivo_alvo)))

  } else {
    arquivos_consolidados <- list.files("dados_caged_parquet", pattern = "CONSOLIDADO", full.names = TRUE)
    if (length(arquivos_consolidados) == 0) {
      cli::cli_alert_danger("Nenhum arquivo consolidado encontrado na pasta.")
      return(invisible(NULL))
    }
    caminho_padrao <- tail(sort(arquivos_consolidados), 1)
    base <- arrow::open_dataset(caminho_padrao)
    cli::cli_alert_success(paste("Dataset consolidado carregado:", basename(caminho_padrao)))
  }

  return(base)
}
