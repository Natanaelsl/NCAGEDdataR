#' NCAGEDdataR: Extração de dados do NOVO CAGED
#'
#' @param ano valor numérico
#' @param mês string para o mês
#'
#' @return
#' @export
#'
#' @examples
NCdata <- function(ano = NULL, mês = NULL){

  # NÃO RETORNAR MENSAGENS DE ERROR
  # options(show.error.messages = FALSE)

  suppressWarnings({

    cli::cli_progress_step("Preparando!", spinner = TRUE)

      if (is.null(ano) | is.null(mês)) {
      cli::cli_alert_danger("O ano e mês devem ser fornecidoS.")
      return(NULL)
    }
    if (ano < 2020) {
      cli::cli_alert_danger("O ano deve ser maior ou igual a 2020.")
      return(NULL)
    }
    if (ano == 2020) {
      cli::cli_div(theme = list(span.emph = list(color = "orange")))
      cli::cli_text("{.emph Desculpa, a extração dO ano de 2020 está em fase de construção.}")
      cli::cli_end()
      # cli::cli_alert_info("Desculpa, a extração dO ano de 2020 está em fase de construção.")
      return(NULL)
    }
    if (ano > timeDate::getRmetricsOptions("currentYear")) {
      cli::cli_alert_danger("O ano não pode ser maior que o ano atual.")
      return(NULL)
    }


    RemoveAcentos <- function(textoComAcentos) {

      # Se nao foi informado texto
      if(!is.character(textoComAcentos)){
        on.exit()
      }

      # Letras com acentos
      letrasComAcentos <- "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨"

      # Letras equivalentes sem acentos
      letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "

      textoSemAcentos <- chartr(
        old = letrasComAcentos,
        new = letrasSemAcentos,
        x = textoComAcentos
      )

    }


    HtmlLink <- paste0("http://pdet.mte.gov.br/novo-caged/novo-caged-", print(ano), "/novo-caged-", print(RemoveAcentos(tolower({{mês}}))),"-", print(ano))


    # CONDIÇÃO PARA OS ÚLTIMOS DADOS DISPONIBILIZADOS
    if (RCurl::url.exists(HtmlLink) == TRUE) {

      cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)
      HTMLContent <- rvest::read_html(HtmlLink)
      xlsx <- HTMLContent %>% rvest::html_nodes("[class='moduletable     listaservico ']") %>%
        rvest::html_elements("a") %>%
        rvest::html_attr("href") %>% .[[3]]

      url <- paste0("http://pdet.mte.gov.br", xlsx)

      cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
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
      mês1 <- HTMLContent2 %>%
        rvest::html_nodes("[class='outstanding-title']") %>%
        rvest::html_text2() %>% .[[1]] %>%
        strsplit(., split = " ") %>%
        unlist() %>% .[4]


      if(RemoveAcentos(tolower({{mês}})) == RemoveAcentos(tolower({{mês1}}))){

        cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

        xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
          rvest::html_elements("a") %>%
          rvest::html_attr("href") %>% .[[3]]

        url <- paste0("http://pdet.mte.gov.br", xlsx2)

        cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
        cli::cli_progress_step("Download realizado!", spinner = TRUE)

        httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

        cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
        # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

        cli::cli_progress_done()

      }
      if(RemoveAcentos(tolower({{mês}})) != RemoveAcentos(tolower({{mês1}}))){
        cli::cli_alert_info("Possível erro na declaração das informações de ano e mês! \nVerificar dados informados.\n")
        pesquisa <- gtools::ask(paste("\nUtilizar última pesquisa disponível? (", mês1, ") \nInforme 'sim' ou 'não'!"))
        pesquisa <- as.character(pesquisa)

        if(pesquisa == "sim"){
          cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

          xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
            rvest::html_elements("a") %>%
            rvest::html_attr("href") %>% .[[3]]

          url <- paste0("http://pdet.mte.gov.br", xlsx2)

          cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
          cli::cli_progress_step("Download realizado!", spinner = TRUE)

          httr::GET(url, httr::write_disk(tf <- tempfile("CAGED", tmpdir = tempdir(), fileext = ".xlsx")), httr::progress(), overwrite = FALSE)

          cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos temporariamente em:", "\n", tf, "\n"))
          # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

          cli::cli_progress_done()

        }
        if(pesquisa == "não"){

          cli::cli_alert_danger("Tudo bem, verifique ano e mês informados!")

        }
      }
    }

    suppressMessages({

      # if(ano == "2020"){}else{}

      if(str_detect(url, glue::glue("{ano}01")) == TRUE | str_detect(url, glue::glue("Jan{ano}")) == TRUE){

        # print("É igual a janeiro")

        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 3)) %>%
          setNames(c(colnames(.)[1], .[1, -1])) %>%
          slice(-1) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado") %>%
          pivot_longer(names_to = "Estados", values_to = "Saldo", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          pivot_wider(names_from = `Grupamento de Atividades Econômicas e Seção CNAE 2.0`, values_from = Saldo) %>%
          gather(`Grupamento de Atividades Econômicas e Seção CNAE 2.0`, valores, -c(1)) %>%
          mutate(valores = as.numeric(valores)) %>%
          rename(!!quo_name(mês) := valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          slice(1:c(nrow(.) - 4))


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          slice(-1) %>%
          mutate(`Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Admissões"
        adm6 <- (str_detect(df_t6[1,], "Admissões"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          slice(-1) %>%
          # setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "Últimos 12 meses"))
          setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6 <- estoque_6 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          bind_cols(
            Admissoes_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = case_when(
          #   !Mês %in% c("Acumulado do Ano", "Últimos 12 meses") ~ lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ Mês)) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        df_t6.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 6.1",
                                      skip = 4,
                                      # progress = readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Estoque"
        est6.1 <- (str_detect(df_t6.1[1,], "Estoque"))

        estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
          slice(-1) %>%
          mutate(`Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Admissões"
        adm6.1 <- (str_detect(df_t6.1[1,], "Admissões"))

        Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Desligamentos"
        desl6.1 <- (str_detect(df_t6.1[1,], "Desligamentos"))

        Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Saldos"
        sal6.1 <- (str_detect(df_t6.1[1,], "Saldos"))

        Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6.1 <- estoque_6.1 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          bind_cols(
            Admissoes_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = case_when(
          #   !Mês %in% c("Acumulado do Ano", "Últimos 12 meses") ~ lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ Mês)) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        df_t7 <- readxl::read_excel(tf,
                                    sheet = "Tabela 7",
                                    skip = 4
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Estoque"
        est7 <- (str_detect(df_t7[1,], "Estoque"))

        estoque_7 <- df_t7[, as.vector(est7)] %>%
          slice(-1) %>%
          mutate(`Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Admissões"
        adm7 <- (str_detect(df_t7[1,], "Admissões"))

        Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Desligamentos"
        desl7 <- (str_detect(df_t7[1,], "Desligamentos"))

        Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Saldos"
        sal7 <- (str_detect(df_t7[1,], "Saldos"))

        Saldos_7 <- df_t7[, as.vector(sal7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7 <- estoque_7 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
          bind_cols(
            Admissoes_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Região e UF` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        df_t7.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 7.1",
                                      skip = 4
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Estoque"
        est7.1 <- (str_detect(df_t7.1[1,], "Estoque"))

        estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
          slice(-1) %>%
          mutate(`Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Admissões"
        adm7.1 <- (str_detect(df_t7.1[1,], "Admissões"))

        Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Desligamentos"
        desl7.1 <- (str_detect(df_t7.1[1,], "Desligamentos"))

        Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Saldos"
        sal7.1 <- (str_detect(df_t7.1[1,], "Saldos"))

        Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7.1 <- estoque_7.1 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
          bind_cols(
            Admissoes_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Região e UF` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))

      } else{

        # ESTRUTURANDO DADOS DA TABELA 4 (SHEET 4)
        Tabela_4 <- readxl::read_excel(tf,
                                       sheet = "Tabela 4",
                                       skip = 4,
                                       progress = readxl::readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 3)) %>%
          setNames(c(colnames(.)[1], .[1, -1])) %>%
          slice(-1) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado") %>%
          pivot_longer(names_to = "Estados", values_to = "Saldo", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          pivot_wider(names_from = `Grupamento de Atividades Econômicas e Seção CNAE 2.0`, values_from = Saldo) %>%
          gather(`Grupamento de Atividades Econômicas e Seção CNAE 2.0`, valores, -c(1)) %>%
          mutate(valores = as.numeric(valores)) %>%
          rename(!!quo_name(mês) := valores)


        # ESTRUTURANDO DADOS DA TABELA 5 (SHEET 5)
        Tabela_5 <- readxl::read_excel(tf,
                                       sheet = "Tabela 5",
                                       skip = 4
        ) %>%
          slice(1:c(nrow(.) - 4))


        # ESTRUTURANDO DADOS DA TABELA 6 (SHEET 6)
        df_t6 <- readxl::read_excel(tf,
                                    sheet = "Tabela 6",
                                    skip = 4,
                                    # progress = readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 7))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Estoque"
        est6 <- (str_detect(df_t6[1,], "Estoque"))

        estoque_6 <- df_t6[, as.vector(est6)] %>%
          # select(c(1, 2, seq(6, ncol(.) - 8, 5))) %>%
          slice(-1) %>%
          mutate(`Acumulado do Ano` = NA,
                 `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Admissões"
        adm6 <- (str_detect(df_t6[1,], "Admissões"))

        Admissoes_6 <- df_t6[, as.vector(adm6)] %>%
          # select(c(1, 3, seq(7, ncol(.) - 8, 5), ncol(.)-7, ncol(.)-3)) %>%
          slice(-1) %>%
          # setNames(c(colnames(estoque_6[,1:c(ncol(estoque_6)-2)]), "Acumulado do Ano", "Últimos 12 meses"))
          setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Desligamentos"
        desl6 <- (str_detect(df_t6[1,], "Desligamentos"))

        Desligamentos_6 <- df_t6[, as.vector(desl6)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6[1,1] <- "Saldos"
        sal6 <- (str_detect(df_t6[1,], "Saldos"))

        Saldos_6 <- df_t6[, as.vector(sal6)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6 <- estoque_6 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          bind_cols(
            Admissoes_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_6 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = case_when(
          #   !Mês %in% c("Acumulado do Ano", "Últimos 12 meses") ~ lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ Mês)) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 6.1 (SHEET 6.1)
        df_t6.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 6.1",
                                      skip = 4,
                                      # progress = readxl_progress()
        ) %>%
          slice(1:c(nrow(.) - 7))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Estoque"
        est6.1 <- (str_detect(df_t6.1[1,], "Estoque"))

        estoque_6.1 <- df_t6.1[, as.vector(est6.1)] %>%
          slice(-1) %>%
          mutate(`Acumulado do Ano` = NA,
                 `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Admissões"
        adm6.1 <- (str_detect(df_t6.1[1,], "Admissões"))

        Admissoes_6.1 <- df_t6.1[, as.vector(adm6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Desligamentos"
        desl6.1 <- (str_detect(df_t6.1[1,], "Desligamentos"))

        Desligamentos_6.1 <- df_t6.1[, as.vector(desl6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t6.1[1,1] <- "Saldos"
        sal6.1 <- (str_detect(df_t6.1[1,], "Saldos"))

        Saldos_6.1 <- df_t6.1[, as.vector(sal6.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_6.1)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_6.1 <- estoque_6.1 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
          bind_cols(
            Admissoes_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_6.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Grupamento de Atividades Econômicas e Seção CNAE 2.0`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = case_when(
          #   !Mês %in% c("Acumulado do Ano", "Últimos 12 meses") ~ lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y")),
          #   TRUE ~ Mês)) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Grupamento de Atividades Econômicas e Seção CNAE 2.0` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7 (SHEET 7)
        df_t7 <- readxl::read_excel(tf,
                                    sheet = "Tabela 7",
                                    skip = 4
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Estoque"
        est7 <- (str_detect(df_t7[1,], "Estoque"))

        estoque_7 <- df_t7[, as.vector(est7)] %>%
          slice(-1) %>%
          mutate(`Acumulado do Ano` = NA,
                 `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Admissões"
        adm7 <- (str_detect(df_t7[1,], "Admissões"))

        Admissoes_7 <- df_t7[, as.vector(adm7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Desligamentos"
        desl7 <- (str_detect(df_t7[1,], "Desligamentos"))

        Desligamentos_7 <- df_t7[, as.vector(desl7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7[1,1] <- "Saldos"
        sal7 <- (str_detect(df_t7[1,], "Saldos"))

        Saldos_7 <- df_t7[, as.vector(sal7)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7 <- estoque_7 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
          bind_cols(
            Admissoes_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_7 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Região e UF` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))



        # ESTRUTURANDO DADOS DA TABELA 7.1 (SHEET 7.1)
        df_t7.1 <- readxl::read_excel(tf,
                                      sheet = "Tabela 7.1",
                                      skip = 4
        ) %>%
          slice(1:c(nrow(.) - 6))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Estoque"
        est7.1 <- (str_detect(df_t7.1[1,], "Estoque"))

        estoque_7.1 <- df_t7.1[, as.vector(est7.1)] %>%
          slice(-1) %>%
          mutate(`Acumulado do Ano` = NA,
                 `Últimos 12 meses` = NA)


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Admissões"
        adm7.1 <- (str_detect(df_t7.1[1,], "Admissões"))

        Admissoes_7.1 <- df_t7.1[, as.vector(adm7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Desligamentos"
        desl7.1 <- (str_detect(df_t7.1[1,], "Desligamentos"))

        Desligamentos_7.1 <- df_t7.1[, as.vector(desl7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # DEFININDO PARÊMETRO PARA SELEÇÃO DAS COLUNAS
        df_t7.1[1,1] <- "Saldos"
        sal7.1 <- (str_detect(df_t7.1[1,], "Saldos"))

        Saldos_7.1 <- df_t7.1[, as.vector(sal7.1)] %>%
          slice(-1) %>%
          setNames(c(colnames(estoque_7.1)))


        # REESTRUTURANDO TABELAS EM UMA ÚNICA
        Tabela_7.1 <- estoque_7.1 %>%
          pivot_longer(names_to = "Mês", values_to = "Estoque", -`Região e UF`) %>%
          bind_cols(
            Admissoes_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Admissoes", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Desligamentos_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Desligamentos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          bind_cols(
            Saldos_7.1 %>%
              pivot_longer(names_to = "Mês", values_to = "Saldos", -`Região e UF`) %>%
              select(!1:2)
          ) %>%
          # dplyr::mutate(Mês = case_when(
          #   Mês == paste0("Abril/", substr(Mês, nchar(Mês) - 4 + 1, nchar(Mês))) ~ gsub("Abril", "April", Mês),
          #   TRUE ~ Mês
          # )) %>%
          # dplyr::mutate(Mês = lubridate::parse_date_time2(Mês, orders = c("%B/%Y", "%B%Y", "%b/%Y"))) %>%
          mutate(
            Estoque = as.numeric(Estoque),
            Admissoes = as.numeric(Admissoes),
            Desligamentos = as.numeric(Desligamentos),
            Saldos = as.numeric(Saldos)
          ) %>%
          # filter(`Região e UF` != "Não identificado***") %>%
          gather(Tipo, valores, -c(1:2))

      }


      # cli::cli_progress_done()
      cli::cli_progress_step("\nOrganizando dados e criando lista de 'Data.frames'!", spinner = TRUE)

      list <- list(Tabela_4=Tabela_4,
                   Tabela_5=Tabela_5,
                   Tabela_6=Tabela_6,
                   Tabela_6.1=Tabela_6.1,
                   Tabela_7=Tabela_7,
                   Tabela_7.1=Tabela_7.1)

      nome <- Tabela_5 %>% mutate(Estoque = as.numeric(.$Estoque)) %>% na.omit() %>% tail(1)
      nome <- tail(nome$Mês, 1)
      # nome <- paste(mês,ano, sep = "_")
      assign(nome, list, env=.GlobalEnv)
      # names(objects(pattern = nome)) <- c("Tabela_4", "Tabela_6", "Tabela_6.1")
      # names(glue::glue("{nome}")) <- c("Tabela_4", "Tabela_6", "Tabela_6.1")


    }) #suppressMessages

  }) #suppressWarnings

}
