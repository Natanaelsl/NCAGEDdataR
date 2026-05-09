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

  googledrive::drive_deauth()
  root_id <- "1F89h6odTPGIGMb9eDiJKCute9W89QmqN"

  # =========================
  # 🔎 BUSCA DE METADADOS (SENSOR)
  # =========================
  # Lista os anos disponíveis no Drive
  anos <- googledrive::drive_ls(googledrive::as_id(root_id)) %>%
    dplyr::filter(!stringr::str_detect(name, "\\."))

  # Determina qual ano investigar
  if(!is.null(ref)) {
    if(ref %in% c("last", "latest")) {
      anos_alvo <- anos %>% dplyr::arrange(dplyr::desc(name)) %>% dplyr::slice(1)
    } else {
      ano_ref <- substr(ref, 1, 4)
      anos_alvo <- anos %>% dplyr::filter(name == ano_ref)
    }
  } else {
    # Se ref for NULL, foca no ano mais recente disponível
    anos_alvo <- anos %>% dplyr::arrange(dplyr::desc(name)) %>% dplyr::slice(1)
  }

  if(nrow(anos_alvo) == 0) return(invisible(NULL))

  # Mapeia os meses/períodos dentro do ano alvo
  estrutura <- purrr::map_df(seq_len(nrow(anos_alvo)), function(i){
    meses <- googledrive::drive_ls(anos_alvo$id[i]) %>%
      dplyr::filter(!stringr::str_detect(name, "\\."))
    tibble::tibble(ym = meses$name, id = meses$id)
  }) %>% dplyr::arrange(ym)

  # Identifica o período final (o mais recente da lista)
  periodo_identificado <- estrutura %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::pull(ym)

  # --- O PONTO CHAVE: SEMÂNTICA DE SENSOR ---
  # Se temp = TRUE, retornamos apenas a string do período (ex: "202604")
  # Isso permite que a função CAGED() decida se precisa baixar ou usar o cache local.
  if(temp) {
    return(periodo_identificado)
  }

  # =========================
  # 📁 PREPARAÇÃO PARA DOWNLOAD REAL
  # =========================
  dir.create(destino, showWarnings = FALSE)
  cli::cli_h1("📥 Download CAGED (RAW)")

  # Filtra a estrutura para o que será baixado de fato
  if(!is.null(ref) && stringr::str_detect(ref, "^\\d{6}$")) {
    estrutura <- estrutura %>% dplyr::filter(ym == ref)
  } else {
    estrutura <- estrutura %>% dplyr::slice_tail(n = 1)
  }

  # Correção da Barra de Progresso (Removendo referência a pb_bar inexistente)
  pb <- cli::cli_progress_bar(
    total = nrow(estrutura),
    format = "Baixando {pb_current}/{pb_total} [{cli::pb_bar}] {pb_percent}"
  )

  # =========================
  # 📥 DOWNLOAD ROBUSTO
  # =========================
  baixar_mes <- function(mes_id, ym){
    arq_dest <- file.path(destino, paste0("CAGED_", ym, ".xlsx"))

    if(file.exists(arq_dest)){
      cli::cli_inform(paste("Arquivo já existe localmente:", ym))
      return(arq_dest)
    }

    arquivos <- tryCatch(googledrive::drive_ls(mes_id), error = function(e) NULL)
    arquivo <- arquivos %>%
      dplyr::filter(stringr::str_detect(name, "\\.xlsx")) %>%
      dplyr::slice(1)

    if(nrow(arquivo) == 0) return(NULL)

    for(i in 1:3){
      ok <- tryCatch({
        googledrive::drive_download(arquivo$id, path = arq_dest, overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)

      if(ok) {
        cli::cli_alert_success(paste("Sucesso:", ym))
        return(arq_dest)
      }
      Sys.sleep(1)
    }
    return(NULL)
  }

  resultados <- vector("list", nrow(estrutura))
  for(i in seq_len(nrow(estrutura))){
    resultados[[i]] <- baixar_mes(estrutura$id[i], estrutura$ym[i])
    cli::cli_progress_update(id = pb)
  }

  cli::cli_progress_done(id = pb)
  return(invisible(unlist(resultados)))
}
