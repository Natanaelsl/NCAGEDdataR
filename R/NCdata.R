#' NCdata: Extração de dados do Novo CAGED
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Realiza o extração dos dados, para o ano e mês especificados, retornando uma lista de 'data.frame' cada uma das abas do aquivo .xlsx dísponibilizado no site do Ministério do Trabalho - PDET.
#'
#' @inheritParams expand
#' @param year Parâmetro numérico. Determina qual o ano do Novo CAGED que será extraido. Deve ser selecionado somente um ano.
#' @param month Parâmetro 'string'. Determina o mês da pesquisa de interesse. Deve ser selecionado somente um mês. Ex.: "Junho".
#' @inheritParams NCdata_
#' @export
#' @examples
#' # install.packages("devtools")
#' # devtools::install_github("Natanaelsl/NCAGEDdataR")
#'
#' # Carregando o pacote
#' # library(NCAGEDdataR)
#'
#' # Extraindo dados de Junho/2023
#' NCdata(2023, "Junho")
NCdata <- function(year = NULL, month = NULL) {

  # stringi::stri_escape_unicode("ê")
  # dang::checkPackageAsciiCode(dir = ".")
  # NÃO RETORNAR MENSAGENS DE ERROR
  # options(show.error.messages = FALSE)

  suppressWarnings({

    # iconv("O ano e mês devem ser fornecidos.", "UTF-8", "ASCII", sub = "c99")

    if (is.null(year) | is.null(month)) {
      cli::cli_alert_danger("O ano e mês devem ser fornecidos.")
      return()
    }
    if (year < 2020) {
      cli::cli_alert_danger("Tente novamente!")
      stop("O ano deve ser maior ou igual a 2020!", call. = FALSE)
    }
    if (year == 2020) {
      # cli::cli_div(theme = list(span.emph = list(color = "orange")))
      # cli::cli_text("{.emph Desculpa, a extra\\u00e7\\u00e3o dOs dados do ano de 2020 est\\u00e1 em fase de constru\\u00e7\\u00e3o.}")
      # cli::cli_end()
      cli::cli_alert_info("Desculpa, a extração dO ano de 2020 está em fase de construção.")
      stop("Selecione outro ano.", call. = FALSE)
    }
    if (year > timeDate::getRmetricsOptions("currentYear")) {
      cli::cli_alert_danger("Tente novamente!")
      stop("O ano não pode ser maior que o ano atual.", call. = FALSE)
    }


    HtmlLink <- paste0("http://pdet.mte.gov.br/novo-caged/novo-caged-", print(year), "/novo-caged-", print(abjutils::rm_accent(tolower({{month}}))),"-", print(year))


    # CONDIÇÃO PARA OS ÚLTIMOS DADOS DISPONIBILIZADOS
    if (RCurl::url.exists(HtmlLink) == TRUE) {

      cli::cli_progress_step("Preparando!", spinner = TRUE)

      cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)
      HTMLContent <- rvest::read_html(HtmlLink)
      xlsx <- HTMLContent %>%
        rvest::html_nodes("[class='moduletable     listaservico ']") %>%
        rvest::html_elements("a") %>%
        rvest::html_attr("href") %>% .[[3]]

      url <- paste0("http://pdet.mte.gov.br",xlsx)

      # cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
      cli::cli_progress_step("Download realizado!", spinner = TRUE)

      httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

      cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
      # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

      cli::cli_progress_done()

    }
    else {

      HtmlLink2 <- "http://pdet.mte.gov.br/novo-caged"
      HTMLContent2 <- rvest::read_html(HtmlLink2)

      # PEGANDO NO TÍTULO MÊS DA ÚLTIMA PESQUISA
      month1 <- HTMLContent2 %>%
        rvest::html_nodes("[class='outstanding-title']") %>%
        rvest::html_text2() %>% .[[1]] %>%
        strsplit(., split = " ") %>%
        unlist() %>% .[4]


      if(abjutils::rm_accent(tolower({{month}})) == abjutils::rm_accent(tolower({{month1}}))){

        cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

        xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
          rvest::html_elements("a") %>%
          rvest::html_attr("href") %>% .[[3]]

        url <- paste0("http://pdet.mte.gov.br",xlsx2)

        # cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
        cli::cli_progress_step("Download realizado!", spinner = TRUE)

        httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

        cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\n", "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
        # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

        cli::cli_progress_done()

      }
      if(abjutils::rm_accent(tolower({{month}})) != abjutils::rm_accent(tolower({{month1}}))){
        # cli::cli_alert_info("Possível erro na declaração das informações de ano e mês! \nVerificar dados informados.\n")
        pesquisa <- gtools::ask(paste("\nUtilizar ultima pesquisa disponível? (", month1, ") \nInforme 'sim' ou 'não'!"))
        pesquisa <- as.character(pesquisa)

        if(pesquisa == "sim"){
          cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

          xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
            rvest::html_elements("a") %>%
            rvest::html_attr("href") %>% .[[3]]

          url <- paste0("http://pdet.mte.gov.br",xlsx2)

          # cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
          cli::cli_progress_step("Download realizado!", spinner = TRUE)

          httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

          cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\n", "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
          # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

          cli::cli_progress_done()

        }
        if(pesquisa == "não"){

          cli::cli_alert_danger("Tudo bem, verifique ano e mês informados!")

        }
      }
    }

    suppressMessages({

    if({{year}} == 2020){
      return(NULL)
    }else{

      if(stringr::str_detect(url, glue::glue("{year}01")) == TRUE | stringr::str_detect(url, glue::glue("Jan{year}")) == TRUE){

        # MUDANDO ESTRUTURA PARA O MÊS DE JANEIRO -> print("É igual a janeiro")
        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 3)) %>%
          stats::setNames(c(colnames(.)[1], .[1, -1])) %>%
          dplyr::slice(-1) %>%
          janitor::clean_names(.) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado") %>%
          tidyr::pivot_longer(names_to = "Estados", values_to = "Saldo", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
          tidyr::pivot_wider(names_from = grupamento_de_atividades_economicas_e_secao_cnae_2_0, values_from = Saldo) %>%
          tidyr::gather(grupamento_de_atividades_economicas_e_secao_cnae_2_0, valores, -c(1)) %>%
          dplyr::mutate(!!dplyr::quo_name(month1) := as.numeric(valores)) %>%
          dplyr::select(-valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 4)) %>%
          janitor::clean_names(.)


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6)) %>%
          janitor::clean_names(.)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (stringr::str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(Ultimos_12_meses = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Admiss"
        adm6 <- (stringr::str_detect(df_t6[1,], "Admiss"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          dplyr::slice(-1) %>%
          # stats::setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "Últimos 12 meses"))
          stats::setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (stringr::str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (stringr::str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6 <- estoque_6 %>%
          tidyr::pivot_longer(names_to = "Mes", values_to = "Estoque", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
          dplyr::bind_cols(
            Admissoes_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Admissoes", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Desligamentos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Saldos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::mutate(Mes = case_when(
            Mes == paste0("Abril/", substr(Mes, nchar(Mes) - 4 + 1, nchar(Mes))) ~ gsub("Abril", "April", Mes),
            TRUE ~ Mes
          )) %>%
          dplyr::mutate(Mes = case_when(
            !Mes %in% c("Acumulado do Ano", "Últimos 12 meses") ~ format(lubridate::parse_date_time2(Mes, orders = c("%B%Y", "%B%Y", "%b/%Y"), lt = FALSE), "%B/%Y"),
            TRUE ~ Mes)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        # df_t6.1 <- readxl::read_excel(tf,
        #                               sheet = "Tabela 6.1",
        #                               skip = 4,
        #                               # progress = readxl_progress()
        # ) %>%
        #   dplyr::slice(1:c(nrow(.) - 6))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t6.1[1,1] <- "Estoque"
        # est6.1 <- (stringr::str_detect(df_t6.1[1,], "Estoque"))
        #
        # estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
        #   dplyr::slice(-1) %>%
        #   dplyr::mutate(`Últimos 12 meses` = NA)
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t6.1[1,1] <- "Admissões"
        # adm6.1 <- (stringr::str_detect(df_t6.1[1,], "Admissões"))
        #
        # Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_6.1)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t6.1[1,1] <- "Desligamentos"
        # desl6.1 <- (stringr::str_detect(df_t6.1[1,], "Desligamentos"))
        #
        # Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_6.1)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t6.1[1,1] <- "Saldos"
        # sal6.1 <- (stringr::str_detect(df_t6.1[1,], "Saldos"))
        #
        # Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_6.1)))
        #
        #
        # # REESTRUTURANDO TABELAS EM UMA ÚNICA
        # Tabela_6.1 <- estoque_6.1 %>%
        #   tidyr::pivot_longer(names_to = "Mês", values_to = "Estoque", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
        #   dplyr::bind_cols(
        #     Admissoes_6.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Desligamentos_6.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Saldos_6.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Saldos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   # dplyr::mutate(Mês = case_when(
        #   #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
        #   #   TRUE ~ Mês
        #   # )) %>%
        #   # dplyr::mutate(Mês = case_when(
        #   #   !Mês %in% c("Acumulado do Ano", "Últimos 12 meses") ~ lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
        #   #   TRUE ~ Mês)) %>%
        #   dplyr::mutate(
        #     Estoque = as.numeric(Estoque),
        #     Admissoes = as.numeric(Admissoes),
        #     Desligamentos = as.numeric(Desligamentos),
        #     Saldos = as.numeric(Saldos)
        #   ) %>%
        #   # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado***") %>%
        #   tidyr::gather(Tipo, valores, -c(1:2))
        #
        #
        #
        # # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        # df_t7 <- readxl::read_excel(tf,
        #                             sheet = "Tabela 7",
        #                             skip = 4
        # ) %>%
        #   dplyr::slice(1:c(nrow(.) - 6))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7[1,1] <- "Estoque"
        # est7 <- (stringr::str_detect(df_t7[1,], "Estoque"))
        #
        # estoque_7 <- df_t7[, as.vector(est7)] %>%
        #   dplyr::slice(-1) %>%
        #   dplyr::mutate(`Últimos 12 meses` = NA)
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7[1,1] <- "Admissões"
        # adm7 <- (stringr::str_detect(df_t7[1,], "Admissões"))
        #
        # Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7[1,1] <- "Desligamentos"
        # desl7 <- (stringr::str_detect(df_t7[1,], "Desligamentos"))
        #
        # Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7[1,1] <- "Saldos"
        # sal7 <- (stringr::str_detect(df_t7[1,], "Saldos"))
        #
        # Saldos_7 <- df_t7[, as.vector(sal7)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7)))
        #
        #
        # # REESTRUTURANDO TABELAS EM UMA ÚNICA
        # Tabela_7 <- estoque_7 %>%
        #   tidyr::pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
        #   dplyr::bind_cols(
        #     Admissoes_7 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Desligamentos_7 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Saldos_7 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   # dplyr::mutate(Mês = case_when(
        #   #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
        #   #   TRUE ~ Mês
        #   # )) %>%
        #   # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
        #   dplyr::mutate(
        #     Estoque = as.numeric(Estoque),
        #     Admissoes = as.numeric(Admissoes),
        #     Desligamentos = as.numeric(Desligamentos),
        #     Saldos = as.numeric(Saldos)
        #   ) %>%
        #   # filter(`Região e UF` != "Não identificado***") %>%
        #   tidyr::gather(Tipo, valores, -c(1:2))
        #
        #
        #
        # # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        # df_t7.1 <- readxl::read_excel(tf,
        #                               sheet = "Tabela 7.1",
        #                               skip = 4
        # ) %>%
        #   dplyr::slice(1:c(nrow(.) - 6))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7.1[1,1] <- "Estoque"
        # est7.1 <- (stringr::str_detect(df_t7.1[1,], "Estoque"))
        #
        # estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
        #   dplyr::slice(-1) %>%
        #   dplyr::mutate(`Últimos 12 meses` = NA)
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7.1[1,1] <- "Admissões"
        # adm7.1 <- (stringr::str_detect(df_t7.1[1,], "Admissões"))
        #
        # Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7.1)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7.1[1,1] <- "Desligamentos"
        # desl7.1 <- (stringr::str_detect(df_t7.1[1,], "Desligamentos"))
        #
        # Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7.1)))
        #
        #
        # # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        # df_t7.1[1,1] <- "Saldos"
        # sal7.1 <- (stringr::str_detect(df_t7.1[1,], "Saldos"))
        #
        # Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
        #   dplyr::slice(-1) %>%
        #   stats::setNames(c(colnames(estoque_7.1)))
        #
        #
        # # REESTRUTURANDO TABELAS EM UMA ÚNICA
        # Tabela_7.1 <- estoque_7.1 %>%
        #   tidyr::pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
        #   dplyr::bind_cols(
        #     Admissoes_7.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Desligamentos_7.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   dplyr::bind_cols(
        #     Saldos_7.1 %>%
        #       tidyr::pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
        #       dplyr::select(!1:2)
        #   ) %>%
        #   # dplyr::mutate(Mês = case_when(
        #   #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
        #   #   TRUE ~ Mês
        #   # )) %>%
        #   # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
        #   dplyr::mutate(
        #     Estoque = as.numeric(Estoque),
        #     Admissoes = as.numeric(Admissoes),
        #     Desligamentos = as.numeric(Desligamentos),
        #     Saldos = as.numeric(Saldos)
        #   ) %>%
        #   # filter(`Região e UF` != "Não identificado***") %>%
        #   tidyr::gather(Tipo, valores, -c(1:2))

      } else{

        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 3)) %>%
          stats::setNames(c(colnames(.)[1], .[1, -1])) %>%
          dplyr::slice(-1) %>%
          janitor::clean_names(.) %>%
          tidyr::pivot_longer(names_to = "Estados", values_to = "Saldo", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
          tidyr::pivot_wider(names_from = grupamento_de_atividades_economicas_e_secao_cnae_2_0, values_from = Saldo) %>%
          tidyr::gather(grupamento_de_atividades_economicas_e_secao_cnae_2_0, valores, -c(1)) %>%
          dplyr::mutate(!!dplyr::quo_name(month) := as.numeric(valores)) %>%
          dplyr::select(-valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 4)) %>%
          janitor::clean_names(.)


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 7)) %>%
          janitor::clean_names(.)



        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (stringr::str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                        `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Admiss"
        adm6 <- (stringr::str_detect(df_t6[1,], "Admiss"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          dplyr::slice(-1) %>%
          # stats::setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "Últimos 12 meses"))
          stats::setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (stringr::str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (stringr::str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6 <- estoque_6 %>%
          tidyr::pivot_longer(names_to = "Mes", values_to = "Estoque", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
          dplyr::bind_cols(
            Admissoes_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Admissoes", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Desligamentos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Saldos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          dplyr::mutate(Mes = dplyr::case_when(
            !Mes %in% c("Acumulado do Ano", "Últimos 12 meses") ~ format(lubridate::parse_date_time2(Mes, orders = c("%B%Y", "%B%Y", "%b/%Y")), "%B/%Y"),
            TRUE ~ Mes)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        df_t6.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 6.1",
                                      skip = 4,
                                      # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 7)) %>%
          janitor::clean_names(.)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Estoque"
        est6.1 <- (stringr::str_detect(df_t6.1[1,], "Estoque"))

        estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                        `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Admiss"
        adm6.1 <- (stringr::str_detect(df_t6.1[1,], "Admiss"))

        Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Desligamentos"
        desl6.1 <- (stringr::str_detect(df_t6.1[1,], "Desligamentos"))

        Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Saldos"
        sal6.1 <- (stringr::str_detect(df_t6.1[1,], "Saldos"))

        Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_6.1)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6.1 <- estoque_6.1 %>%
          tidyr::pivot_longer(names_to = "Mes", values_to = "Estoque", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
          dplyr::bind_cols(
            Admissoes_6.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Admissoes", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Desligamentos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Saldos", -grupamento_de_atividades_economicas_e_secao_cnae_2_0) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::mutate(Mes = dplyr::case_when(
            !Mes %in% c("Acumulado do Ano", "Últimos 12 meses") ~ format(lubridate::parse_date_time2(Mes, orders = c("%B%Y", "%B%Y", "%b/%Y")), "%B/%Y"),
            TRUE ~ Mes)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        df_t7 <- readxl::read_excel(tf,
                                    sheet = "Tabela 7",
                                    skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6)) %>%
          janitor::clean_names(.)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Estoque"
        est7 <- (stringr::str_detect(df_t7[1,], "Estoque"))

        estoque_7 <- df_t7[, as.vector(est7)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                        `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Admiss"
        adm7 <- (stringr::str_detect(df_t7[1,], "Admiss"))

        Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Desligamentos"
        desl7 <- (stringr::str_detect(df_t7[1,], "Desligamentos"))

        Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Saldos"
        sal7 <- (stringr::str_detect(df_t7[1,], "Saldos"))

        Saldos_7 <- df_t7[, as.vector(sal7)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7 <- estoque_7 %>%
          tidyr::pivot_longer(names_to = "Mes", values_to = "Estoque", -regiao_e_uf) %>%
          dplyr::bind_cols(
            Admissoes_7 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Admissoes", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Desligamentos", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Saldos", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::mutate(Mes = dplyr::case_when(
            !Mes %in% c("Acumulado do Ano", "Últimos 12 meses") ~ format(lubridate::parse_date_time2(Mes, orders = c("%B%Y", "%B%Y", "%b/%Y")), "%B/%Y"),
            TRUE ~ Mes)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        df_t7.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 7.1",
                                      skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6)) %>%
          janitor::clean_names(.)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Estoque"
        est7.1 <- (stringr::str_detect(df_t7.1[1,], "Estoque"))

        estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                        `Últimos 12 meses` = NA) %>%
          stats::setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Admiss"
        adm7.1 <- (stringr::str_detect(df_t7.1[1,], "Admiss"))

        Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Desligamentos"
        desl7.1 <- (stringr::str_detect(df_t7.1[1,], "Desligamentos"))

        Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Saldos"
        sal7.1 <- (stringr::str_detect(df_t7.1[1,], "Saldos"))

        Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
          dplyr::slice(-1) %>%
          stats::setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7.1 <- estoque_7.1 %>%
          tidyr::pivot_longer(names_to = "Mes", values_to = "Estoque", -regiao_e_uf) %>%
          dplyr::bind_cols(
            Admissoes_7.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Admissoes", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Desligamentos", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7.1 %>%
              tidyr::pivot_longer(names_to = "Mes", values_to = "Saldos", -regiao_e_uf) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::mutate(Mes = dplyr::case_when(
            !Mes %in% c("Acumulado do Ano", "Últimos 12 meses") ~ format(lubridate::parse_date_time2(Mes, orders = c("%B%Y", "%B%Y", "%b/%Y")), "%B/%Y"),
            TRUE ~ Mes)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          tidyr::gather(Tipo, valores, -c(1:2))

      }

    }


      # cli::cli_progress_done()
      cli::cli_progress_step("\nOrganizando dados e criando lista de 'Data.frames'!", spinner = TRUE)

      list <- list(Tabela_4=Tabela_4,
                   Tabela_5=Tabela_5,
                   Tabela_6=Tabela_6,
                   Tabela_6.1=Tabela_6.1,
                   Tabela_7=Tabela_7,
                   Tabela_7.1=Tabela_7.1
                   )

      nome <- Tabela_5 %>% dplyr::mutate(estoque = as.numeric(.$estoque)) %>% stats::na.omit() %>% utils::tail(1)
      nome <- utils::tail(nome$mes, 1)

      return(assign(glue::glue("{nome}"), list, envir=.GlobalEnv))


      cli::cli_progress_done()

    }) #suppressMessages

  }) #suppressWarnings

}
