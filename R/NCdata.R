#' NCAGEDdataR: Extra????o de dados do NOVO CAGED
#'
#''
#' @param year valor num??rico
#' @param month string para o m??s
#'
#' @return Uma lista de data.frame
#' @export
NCdata <- function(year = NULL, month = NULL) {

  # dang::checkPackageAsciiCode(dir = ".")
  # N??O RETORNAR MENSAGENS DE ERROR
  # options(show.error.messages = FALSE)

  suppressWarnings({

    cli::cli_progress_step("Preparando!", spinner = TRUE)

      if (is.null(year) | is.null(month)) {
      cli::cli_alert_danger("O ano e m??s devem ser fornecidoS.")
      return(NULL)
    }
    if (year < 2020) {
      cli::cli_alert_danger("O ano deve ser maior ou igual a 2020.")
      return(NULL)
    }
    if (year == 2020) {
      cli::cli_div(theme = list(span.emph = list(color = "orange")))
      cli::cli_text("{.emph Desculpa, a extra????o dO ano de 2020 est?? em fase de constru????o.}")
      cli::cli_end()
      # cli::cli_alert_info("Desculpa, a extra????o dO ano de 2020 est?? em fase de constru????o.")
      return(NULL)
    }
    if (year > timeDate::getRmetricsOptions("currentYear")) {
      cli::cli_alert_danger("O ano n??o pode ser maior que o ano atual.")
      return(NULL)
    }


    RemoveAcentos <- function(textoComAcentos) {
      # Se nao foi informado texto
      if(!is.character(textoComAcentos)){
        on.exit()
      }

      # Letras com acentos
      letrasComAcentos <- "????????????????????????????????????????????????????????????????????????????????????????????????????????`^~??"

      # Letras equivalentes sem acentos
      letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "

      textoSemAcentos <- chartr(
        old = letrasComAcentos,
        new = letrasSemAcentos,
        x = textoComAcentos
      )
    }


    HtmlLink <- paste0("http://pdet.mte.gov.br/novo-caged/novo-caged-", print(year), "/novo-caged-", print(RemoveAcentos(tolower({{month}}))),"-", print(year))


    # CONDI????O PARA OS ??LTIMOS DADOS DISPONIBILIZADOS
    if (RCurl::url.exists(HtmlLink) == TRUE) {

      cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)
      HTMLContent <- rvest::read_html(HtmlLink)
      xlsx <- HTMLContent %>% rvest::html_nodes("[class='moduletable     listaservico ']") %>%
        rvest::html_elements("a") %>%
        rvest::html_attr("href") %>% .[[3]]

      url <- paste0("http://pdet.mte.gov.br", xlsx)

      cli::cli_progress_step("Come??ando o Download dos Dados CAGED", spinner = TRUE)
      cli::cli_progress_step("Download realizado!", spinner = TRUE)

      httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

      cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
      # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

      cli::cli_progress_done()

    }
    else {

      HtmlLink2 <- "http://pdet.mte.gov.br/novo-caged"
      HTMLContent2 <- rvest::read_html(HtmlLink2)

      # PEGANDO NO T??TULO M??S DA ??LTIMA PESQUISA
      month1 <- HTMLContent2 %>%
        rvest::html_nodes("[class='outstanding-title']") %>%
        rvest::html_text2() %>% .[[1]] %>%
        strsplit(., split = " ") %>%
        unlist() %>% .[4]


      if(RemoveAcentos(tolower({{month}})) == RemoveAcentos(tolower({{month1}}))){

        cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

        xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
          rvest::html_elements("a") %>%
          rvest::html_attr("href") %>% .[[3]]

        url <- paste0("http://pdet.mte.gov.br", xlsx2)

        cli::cli_progress_step("Come??ando o Download dos Dados CAGED", spinner = TRUE)
        cli::cli_progress_step("Download realizado!", spinner = TRUE)

        httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

        cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
        # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

        cli::cli_progress_done()

      }
      if(RemoveAcentos(tolower({{month}})) != RemoveAcentos(tolower({{month1}}))){
        cli::cli_alert_info("Poss??vel erro na declara????o das informa????es de ano e m??s! \nVerificar dados informados.\n")
        pesquisa <- gtools::ask(paste("\nUtilizar ??ltima pesquisa dispon??vel? (", month1, ") \nInforme 'sim' ou 'n??o'!"))
        pesquisa <- as.character(pesquisa)

        if(pesquisa == "sim"){
          cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

          xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
            rvest::html_elements("a") %>%
            rvest::html_attr("href") %>% .[[3]]

          url <- paste0("http://pdet.mte.gov.br", xlsx2)

          cli::cli_progress_step("Come??ando o Download dos Dados CAGED", spinner = TRUE)
          cli::cli_progress_step("Download realizado!", spinner = TRUE)

          httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

          cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
          # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

          cli::cli_progress_done()

        }
        if(pesquisa == "n??o"){

          cli::cli_alert_danger("Tudo bem, verifique ano e m??s informados!")

        }
      }
    }

    suppressMessages({

      # if(ano == "2020"){}else{}

      if(stringr::str_detect(url, glue::glue("{year}01")) == TRUE | stringr::str_detect(url, glue::glue("Jan{year}")) == TRUE){

        # print("?? igual a janeiro")

        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 3)) %>%
          setNames(c(colnames(.)[1], .[1, -1])) %>%
          dplyr::slice(-1) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado") %>%
          tidyr::pivot_longer(names_to = "Estados", values_to = "Saldo", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          tidyr::pivot_wider(names_from = `Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`, values_from = Saldo) %>%
          tidyr::gather(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`, valores, -c(1)) %>%
          dplyr::mutate(valores = as.numeric(valores)) %>%
          dplyr::rename(!!dplyr::quo_name(month) := valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 4))


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (stringr::str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Admiss??es"
        adm6 <- (stringr::str_detect(df_t6[1,], "Admiss??es"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          dplyr::slice(-1) %>%
          # setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "??ltimos 12 meses"))
          setNames(c(colnames(estoque_6)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (stringr::str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (stringr::str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_6 <- estoque_6 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          dplyr::bind_cols(
            Admissoes_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = case_when(
          #   !M??s %in% c("Acumulado do Ano", "??ltimos 12 meses") ~ lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ M??s)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        df_t6.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 6.1",
                                      skip = 4,
                                      # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Estoque"
        est6.1 <- (stringr::str_detect(df_t6.1[1,], "Estoque"))

        estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Admiss??es"
        adm6.1 <- (stringr::str_detect(df_t6.1[1,], "Admiss??es"))

        Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Desligamentos"
        desl6.1 <- (stringr::str_detect(df_t6.1[1,], "Desligamentos"))

        Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Saldos"
        sal6.1 <- (stringr::str_detect(df_t6.1[1,], "Saldos"))

        Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_6.1 <- estoque_6.1 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          dplyr::bind_cols(
            Admissoes_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = case_when(
          #   !M??s %in% c("Acumulado do Ano", "??ltimos 12 meses") ~ lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ M??s)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        df_t7 <- readxl::read_excel(tf,
                                    sheet = "Tabela 7",
                                    skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Estoque"
        est7 <- (stringr::str_detect(df_t7[1,], "Estoque"))

        estoque_7 <- df_t7[, as.vector(est7)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Admiss??es"
        adm7 <- (stringr::str_detect(df_t7[1,], "Admiss??es"))

        Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Desligamentos"
        desl7 <- (stringr::str_detect(df_t7[1,], "Desligamentos"))

        Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Saldos"
        sal7 <- (stringr::str_detect(df_t7[1,], "Saldos"))

        Saldos_7 <- df_t7[, as.vector(sal7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_7 <- estoque_7 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Regi??o e UF`) %>%
          dplyr::bind_cols(
            Admissoes_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Regi??o e UF` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        df_t7.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 7.1",
                                      skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Estoque"
        est7.1 <- (stringr::str_detect(df_t7.1[1,], "Estoque"))

        estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Admiss??es"
        adm7.1 <- (stringr::str_detect(df_t7.1[1,], "Admiss??es"))

        Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Desligamentos"
        desl7.1 <- (stringr::str_detect(df_t7.1[1,], "Desligamentos"))

        Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Saldos"
        sal7.1 <- (stringr::str_detect(df_t7.1[1,], "Saldos"))

        Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_7.1 <- estoque_7.1 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Regi??o e UF`) %>%
          dplyr::bind_cols(
            Admissoes_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Regi??o e UF` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))

      } else{

        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 3)) %>%
          setNames(c(colnames(.)[1], .[1, -1])) %>%
          dplyr::slice(-1) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado") %>%
          tidyr::pivot_longer(names_to = "Estados", values_to = "Saldo", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          tidyr::pivot_wider(names_from = `Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`, values_from = Saldo) %>%
          tidyr::gather(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`, valores, -c(1)) %>%
          dplyr::mutate(valores = as.numeric(valores)) %>%
          dplyr::rename(!!dplyr::quo_name(month) := valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 4))


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 7))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (stringr::str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                 `??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Admiss??es"
        adm6 <- (stringr::str_detect(df_t6[1,], "Admiss??es"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          dplyr::slice(-1) %>%
          # setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "??ltimos 12 meses"))
          setNames(c(colnames(estoque_6)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (stringr::str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (stringr::str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_6 <- estoque_6 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          dplyr::bind_cols(
            Admissoes_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = case_when(
          #   !M??s %in% c("Acumulado do Ano", "??ltimos 12 meses") ~ lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ M??s)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        df_t6.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 6.1",
                                      skip = 4,
                                      # progress = readxl_progress()
        ) %>%
          dplyr::slice(1:c(nrow(.) - 7))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Estoque"
        est6.1 <- (stringr::str_detect(df_t6.1[1,], "Estoque"))

        estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                 `??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Admiss??es"
        adm6.1 <- (stringr::str_detect(df_t6.1[1,], "Admiss??es"))

        Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Desligamentos"
        desl6.1 <- (stringr::str_detect(df_t6.1[1,], "Desligamentos"))

        Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t6.1[1,1] <- "Saldos"
        sal6.1 <- (stringr::str_detect(df_t6.1[1,], "Saldos"))

        Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_6.1 <- estoque_6.1 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
          dplyr::bind_cols(
            Admissoes_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_6.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = case_when(
          #   !M??s %in% c("Acumulado do Ano", "??ltimos 12 meses") ~ lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ M??s)) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econ??micas e Se????o CNAE 2.0` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        df_t7 <- readxl::read_excel(tf,
                                    sheet = "Tabela 7",
                                    skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Estoque"
        est7 <- (stringr::str_detect(df_t7[1,], "Estoque"))

        estoque_7 <- df_t7[, as.vector(est7)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                 `??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Admiss??es"
        adm7 <- (stringr::str_detect(df_t7[1,], "Admiss??es"))

        Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Desligamentos"
        desl7 <- (stringr::str_detect(df_t7[1,], "Desligamentos"))

        Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7[1,1] <- "Saldos"
        sal7 <- (stringr::str_detect(df_t7[1,], "Saldos"))

        Saldos_7 <- df_t7[, as.vector(sal7)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_7 <- estoque_7 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Regi??o e UF`) %>%
          dplyr::bind_cols(
            Admissoes_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Regi??o e UF` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        df_t7.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 7.1",
                                      skip = 4
        ) %>%
          dplyr::slice(1:c(nrow(.) - 6))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Estoque"
        est7.1 <- (stringr::str_detect(df_t7.1[1,], "Estoque"))

        estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
          dplyr::slice(-1) %>%
          dplyr::mutate(`Acumulado do Ano` = NA,
                 `??ltimos 12 meses` = NA)


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Admiss??es"
        adm7.1 <- (stringr::str_detect(df_t7.1[1,], "Admiss??es"))

        Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Desligamentos"
        desl7.1 <- (stringr::str_detect(df_t7.1[1,], "Desligamentos"))

        Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PAR??METRO PARA SELE????O DAS COLUNAS
        df_t7.1[1,1] <- "Saldos"
        sal7.1 <- (stringr::str_detect(df_t7.1[1,], "Saldos"))

        Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
          dplyr::slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # REESTRUTURANDO TABELAS EM UMA ??NICA
        Tabela_7.1 <- estoque_7.1 %>%
          tidyr::pivot_longer(names_to = "M??s", values_to = "Estoque", -`Regi??o e UF`) %>%
          dplyr::bind_cols(
            Admissoes_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Admissoes", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Desligamentos_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Desligamentos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          dplyr::bind_cols(
            Saldos_7.1 %>%
              tidyr::pivot_longer(names_to = "M??s", values_to = "Saldos", -`Regi??o e UF`) %>%
              dplyr::select(!1:2)
          ) %>%
          # dplyr::mutate(M??s = case_when(
          #   M??s == paste0("Abril/", substr(M??s, nchar(M??s) - 4 + 1, nchar(M??s))) ~ gsub("Abril", "April", M??s),
          #   TRUE ~ M??s
          # )) %>%
          # dplyr::mutate(M??s = lubridate::parse_date_time2(M??s, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          dplyr::mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Regi??o e UF` != "N??o identificado***") %>%
          tidyr::gather(Tipo, valores, -c(1:2))

      }


      # cli::cli_progress_done()
      cli::cli_progress_step("\nOrganizando dados e criando lista de 'Data.frames'!", spinner = TRUE)

      list <- list(Tabela_4=Tabela_4,
                   Tabela_5=Tabela_5,
                   Tabela_6=Tabela_6,
                   Tabela_6.1=Tabela_6.1,
                   Tabela_7=Tabela_7,
                   Tabela_7.1=Tabela_7.1)

      nome <- Tabela_5 %>% dplyr::mutate(Estoque = as.numeric(.$Estoque)) %>% na.omit() %>% tail(1)
      nome <- tail(nome$`M??s`, 1)
      # nome <- paste(m??s,ano, sep = "_")
      return(assign(nome, list, envir=.GlobalEnv))
      # names(objects(pattern = nome)) <- c("Tabela_4", "Tabela_6", "Tabela_6.1")
      # names(glue::glue("{nome}")) <- c("Tabela_4", "Tabela_6", "Tabela_6.1")


    }) #suppressMessages

  }) #suppressWarnings

}