#' Download de Arquivos Brutos do Novo CAGED
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Realiza a extração direta dos arquivos Excel (`.xlsx`) brutos do repositório em nuvem
#' oficial (Google Drive), com busca otimizada para evitar bloqueios de API.
#'
#' @param ref String. Aceita "last" para a última competência, ou formatos numéricos como "2026" ou "202603".
#' @param temp Lógico. Se TRUE, salva o arquivo na pasta volátil temporária do servidor/máquina (`tempdir()`).
#' @param destino Caminho do diretório onde o arquivo será salvo caso `temp` seja FALSE.
#'
#' @return Retorna de forma invisível o caminho completo (path) onde o arquivo `.xlsx` foi salvo no disco.
#'
#' @export
# 1. FUNÇÃO DE DOWNLOAD (Otimizada para API) ----
download_caged <- function(ref = NULL,
                           destino = "dados_caged_raw",
                           temp = FALSE) {

  # suppressPackageStartupMessages({
  #   library(googledrive)
  #   library(dplyr)
  #   library(stringr)
  #   library(cli)
  #   library(purrr)
  # })

  googledrive::drive_deauth()
  root_id <- "1F89h6odTPGIGMb9eDiJKCute9W89QmqN"

  # =========================
  # 📁 DESTINO
  # =========================
  if(temp){
    # Garante que o arquivo vá para a raiz do tempdir para facilitar a busca depois
    destino <- tempdir()
    cli::cli_alert_info(paste("Modo temporário ativado. Destino:", destino))
  } else {
    dir.create(destino, showWarnings = FALSE)
  }

  cli::cli_h1("📥 Download CAGED (RAW)")

  # =========================
  # 🔎 BUSCA INTELIGENTE NA API
  # =========================
  anos <- googledrive::drive_ls(googledrive::as_id(root_id)) %>% dplyr::filter(!stringr::str_detect(name, "\\."))

  # Determina qual ano investigar para economizar chamadas na API do Drive
  if(!is.null(ref)) {
    if(ref %in% c("last", "latest")) {
      anos_alvo <- anos %>% dplyr::arrange(dplyr::desc(name)) %>% dplyr::slice(1)
    } else if(stringr::str_detect(ref, "^\\d{6}$") || stringr::str_detect(ref, "^\\d{4}$")) {
      ano_ref <- substr(ref, 1, 4)
      anos_alvo <- anos %>% dplyr::filter(name == ano_ref)
    } else {
      stop("Use: YYYY, YYYYMM ou 'last'")
    }
  } else {
    anos_alvo <- anos
  }

  if(nrow(anos_alvo) == 0){
    cli::cli_alert_warning("Ano não encontrado no Drive.")
    return(invisible(NULL))
  }

  # Mapeia apenas as pastas do ano(s) alvo
  estrutura <- purrr::map_df(seq_len(nrow(anos_alvo)), function(i){
    meses <- googledrive::drive_ls(anos_alvo$id[i]) %>% dplyr::filter(!stringr::str_detect(name, "\\."))
    tibble(ym = meses$name, id = meses$id)
  }) %>% dplyr::arrange(ym)

  # Refina o filtro para o mês específico (se aplicável)
  if(!is.null(ref)) {
    if(ref %in% c("last", "latest")) {
      estrutura <- estrutura %>% dplyr::slice_tail(n = 1)
      cli::cli_alert_info(paste("Último período identificado:", estrutura$ym))
    } else if(stringr::str_detect(ref, "^\\d{6}$")) {
      estrutura <- estrutura %>% dplyr::filter(ym == ref)
    }
  }

  if(nrow(estrutura) == 0){
    cli::cli_alert_warning("Período específico não encontrado.")
    return(invisible(NULL))
  }

  pb <- cli::cli_progress_bar(
    total  = nrow(estrutura),
    format = "Download [{pb_bar}] {pb_percent} | {pb_current}/{pb_total}"
  )

  # =========================
  # 📥 DOWNLOAD ROBUSTO
  # =========================
  baixar_mes <- function(mes_id, ym){
    arq_dest <- file.path(destino, paste0("CAGED_", ym, ".xlsx"))

    if(!temp && file.exists(arq_dest)){
      cli::cli_inform(paste("Arquivo já existe no cache local:", ym))
      return(arq_dest)
    }

    arquivos <- tryCatch(googledrive::drive_ls(mes_id), error = function(e) NULL)
    if(is.null(arquivos)) return(NULL)

    arquivo <- arquivos %>% dplyr::filter(stringr::str_detect(name, "\\.xlsx")) %>% dplyr::slice(1)
    if(nrow(arquivo) == 0) return(NULL)

    for(i in 1:3){
      ok <- tryCatch({
        googledrive::drive_download(arquivo$id, path = arq_dest, overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)

      if(ok) {
        cli::cli_alert_success(paste("Baixado com sucesso:", ym))
        return(arq_dest)
      }
      Sys.sleep(1)
    }
    cli::cli_alert_danger(paste("Falha após 3 tentativas:", ym))
    return(NULL)
  }

  resultados <- vector("list", nrow(estrutura))
  for(i in seq_len(nrow(estrutura))){
    resultados[[i]] <- baixar_mes(estrutura$id[i], estrutura$ym[i])
    cli::cli_progress_update(id = pb, inc = 1)
  }

  cli::cli_progress_done(id = pb)
  validos <- sum(!sapply(resultados, is.null))
  cli::cli_alert_success(paste("Arquivos válidos baixados:", validos))

  return(invisible(unlist(resultados)))
}
