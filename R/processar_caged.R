#' Motor Analítico de Processamento do Novo CAGED
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' A função `processar_caged()` atua como o motor interno de transformação do pacote.
#' Ela lê os arquivos originais `.xlsx` (com as 11 abas distintas), padroniza a estrutura
#' dos cabeçalhos, lida com formatações dinâmicas e consolida os dados em uma arquitetura
#' tabular otimizada. O resultado é persistido em disco utilizando o formato colunar Apache Parquet.
#'
#' @param usar_temporario Lógico. Define a origem dos dados. Se `TRUE`, a função processará
#'   arquivos recém-baixados na memória volátil (`tempdir()`). Se `FALSE`, processará
#'   arquivos em um diretório local permanente (útil para desenvolvimento ou auditoria).
#' @param origem String. Caminho do diretório de onde o arquivo `.xlsx` será lido. Se `NULL`
#'   e `usar_temporario` for `TRUE`, o pacote buscará na pasta temporária. Utilizado
#'   internamente para testes com mock data (ex: miniatura do CAGED).
#' @param destino String. Caminho do diretório onde os arquivos `.parquet` finais
#'   serão salvos. Se `NULL`, salva no diretório de trabalho atual ou pasta temporária.
#' @param parquet_individual Lógico. Controle de saída. Se `TRUE`, gera um arquivo `.parquet`
#'   independente para cada aba do Excel (ideal para modelagem de BI corporativo).
#'   Se `FALSE` (padrão), une todas as tabelas em um único dataset consolidado.
#'
#' @return Retorna invisivelmente uma conexão/Dataset (`arrow::open_dataset()`)
#'   apontando para os arquivos processados, pronto para consultas *Lazy Evaluation*.
#'   Se nenhum dado for processado, retorna `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Processamento de um arquivo local para geração de base consolidada
#' # base_pronta <- processar_caged(
#' #   usar_temporario = FALSE,
#' #   origem = "pasta_com_excel_caged/",
#' #   parquet_individual = FALSE
#' # )
#'
#' # Gerando arquivos independentes para Power BI / Tableau
#' # processar_caged(parquet_individual = TRUE)
#' }
# 2. FUNÇÃO DE PROCESSAMENTO (Código DRY) ----
processar_caged <- function(usar_temporario = FALSE,
                            parquet_individual = FALSE,
                            origem = "dados_caged_raw",
                            destino = "dados_caged_parquet") {

  # suppressPackageStartupMessages({
  #   library(readxl)
  #   library(dplyr)
  #   library(stringr)
  #   library(tidyr)
  #   library(purrr)
  #   library(arrow)
  #   library(cli)
  # })

  # --- CONFIGURAÇÃO DE CAMINHO ---
  if (usar_temporario) {
    pasta_busca <- tempdir()
    cli::cli_alert_info("Modo Temp Automático: Buscando na memória do sistema...")
  } else {
    pasta_busca <- origem
  }

  # Ajuste Crítico: Verifica se 'origem' é um arquivo direto (Mock) ou uma pasta
  if (file.exists(pasta_busca) && !dir.exists(pasta_busca)) {
    caminho_arquivo <- pasta_busca
  } else {
    arquivos <- list.files(pasta_busca, full.names = TRUE, pattern = "xlsx$", recursive = TRUE)

    if (length(arquivos) == 0) {
      cli::cli_alert_danger("Nenhum arquivo .xlsx encontrado em: {pasta_busca}")
      return(invisible(NULL))
    }

    # Seleciona o arquivo mais recente na pasta
    info_arquivos <- file.info(arquivos)
    caminho_arquivo <- rownames(info_arquivos)[which.max(info_arquivos$mtime)]
  }

  cli::cli_h1("⚙️ Processando Base: {basename(caminho_arquivo)}")

  # --- SUBFUNÇÃO DE PADRONIZAÇÃO ---
  padronizar_cabecalho <- function(df_local, colunas_chave) {
    indice_corte <- which(stringr::str_detect(stringr::str_trim(df_local[[1]]), "(?i)^Não identificado"))[1]
    if(!is.na(indice_corte)) df_limpo <- df_local %>% dplyr::slice(1:indice_corte) else df_limpo <- df_local

    nomes_periodo <- names(df_limpo)
    nomes_periodo <- ifelse(grepl("^\\.\\.\\.", nomes_periodo), NA, nomes_periodo)
    for(i in 2:length(nomes_periodo)) if(is.na(nomes_periodo[i])) nomes_periodo[i] <- nomes_periodo[i-1]

    nomes_variaveis <- as.character(df_limpo[1, ])
    nomes_finais <- paste(nomes_periodo, nomes_variaveis, sep = "___")

    nomes_finais[1:length(colunas_chave)] <- colunas_chave
    names(df_limpo) <- nomes_finais
    df_limpo <- df_limpo[-1, ]

    df_limpo %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(colunas_chave), names_to = c("Periodo", "Metrica"), names_sep = "___", values_to = "Valor") %>%
      dplyr::mutate(Valor = suppressWarnings(as.numeric(Valor)),
                    Periodo = stringr::str_remove_all(Periodo, " - sem ajustes?| - com ajustes?|\\*\\*.*") %>% stringr::str_trim())
  }

  # --- LÓGICA DE LIMPEZA DINÂMICA ---
  limpar_caged_dinamico <- function(df, nome_aba) {
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    nome_aba <- stringr::str_trim(nome_aba)

    if (stringr::str_detect(nome_aba, "^Tabela 1|^Tabela 6|^Tabela 10")) {
      return(padronizar_cabecalho(df, c("Grupamento_CNAE")))
    } else if (stringr::str_detect(nome_aba, "^Tabela 2|^Tabela 7|^Tabela 11")) {
      return(padronizar_cabecalho(df, c("Regiao_UF")))
    } else if (stringr::str_detect(nome_aba, "^Tabela 3|^Tabela 8")) {
      return(padronizar_cabecalho(df, c("UF", "Codigo_Municipio", "Municipio")))
    } else if (stringr::str_detect(nome_aba, "^Tabela 4")) {
      return(padronizar_cabecalho(df, c("Categoria")))
    } else if (stringr::str_detect(nome_aba, "^Tabela 5")) {
      indice_corte <- which(stringr::str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% dplyr::slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      return(df_limpo[-1, ] %>% tidyr::pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               dplyr::mutate(Valor = suppressWarnings(as.numeric(Valor)), Periodo = stringr::str_remove_all(Periodo, "\\*\\*.*|\\*") %>% stringr::str_trim()))
    } else if (stringr::str_detect(nome_aba, "^Tabela 9")) {
      indice_corte <- which(stringr::str_detect(df[[1]], "(?i)^Fonte|^Nota"))[1]
      if (!is.na(indice_corte)) df_limpo <- df %>% dplyr::slice(1:(indice_corte - 1)) else df_limpo <- df
      nomes_finais <- as.character(df_limpo[1, ])
      nomes_finais[1] <- "Periodo"
      names(df_limpo) <- nomes_finais
      return(df_limpo[-1, ] %>% tidyr::pivot_longer(cols = -Periodo, names_to = "Metrica", values_to = "Valor") %>%
               dplyr::mutate(Valor = suppressWarnings(ifelse(stringr::str_detect(Valor, "R\\$"), as.numeric(stringr::str_replace(stringr::str_remove_all(Valor, "R\\$\\s*|\\."), ",", ".")), as.numeric(Valor))),
                             Periodo = stringr::str_remove_all(Periodo, "\\*\\*.*|\\*") %>% stringr::str_trim()))
    }
    return(NULL)
  }

  # --- EXECUÇÃO ---
  abas <- readxl::excel_sheets(caminho_arquivo)

  lista_tabelas <- purrr::map(abas, function(aba) {
    df_bruto <- tryCatch(suppressMessages(readxl::read_excel(caminho_arquivo, sheet = aba, skip = 4)), error = function(e) NULL)
    if (is.null(df_bruto) || nrow(df_bruto) == 0) return(NULL)
    df_limpo <- limpar_caged_dinamico(df_bruto, aba)
    if (!is.null(df_limpo)) return(df_limpo %>% dplyr::mutate(Tabela_Origem = aba)) else return(NULL)
  })

  names(lista_tabelas) <- abas
  lista_tabelas <- lista_tabelas[!sapply(lista_tabelas, is.null)]

  dir.create(destino, showWarnings = FALSE)
  ym <- stringr::str_extract(basename(caminho_arquivo), "\\d{4,6}")
  if(is.na(ym)) ym <- "ATUAL"

  if (parquet_individual) {
    purrr::iwalk(lista_tabelas, ~{
      nome_safe <- stringr::str_replace_all(.y, "[^a-zA-Z0-9]", "_")
      arrow::write_parquet(.x, file.path(destino, paste0("CAGED_", nome_safe, "_", ym, ".parquet")))
    })
    cli::cli_alert_success("Concluído! Tabelas individuais em: {destino}")
    return(invisible(lista_tabelas))
  } else {
    df_consolidado <- dplyr::bind_rows(lista_tabelas)
    caminho_final <- file.path(destino, paste0("CAGED_CONSOLIDADO_", ym, ".parquet"))
    arrow::write_parquet(df_consolidado, caminho_final)
    cli::cli_alert_success("Concluído! Base unificada em: {basename(caminho_final)}")

    # Ajuste Crítico: Retorna o Dataset Arrow para o teste e para o usuário
    return(arrow::open_dataset(caminho_final))
  }
}
