#' NCdown: Download do arquivo .xlsx do Novo CAGED
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Realiza o download do aquivo .xlsx no site do Ministério do Trabalho - PDET com os dados do NOVO CAGED para o ano e mês especificados.
#'
#' @inheritParams expand
#' @param year Parâmetro numérico. Determina o ano do download do Novo CAGED. Deve ser selecionado somente um ano.
#' @param month Parâmetro 'string'. Determina o mês do download da pesquisa de interesse. Deve ser selecionado somente um mês. Ex.: "Junho".
#' @param path Diretório onde o aquivo .xlsx do PDET com os dados do NOVO CAGED será salvo.
#' @inheritParams NCdown_
#' @export
#' @examples
#' # install.packages("devtools")
#' # devtools::install_github("Natanaelsl/NCAGEDdataR")
#'
#' # Carregando o pacote
#' # library(NCAGEDdataR)
#'
#' # Definindo download para a pasta do diretório em uso
#' NCdown(2023, "Junho", " ")
#'
#' # Definindo pasta específica para salvar o arquivo
#' NCdown(2023, "Junho", path = paste0(Sys.getenv("USERPROFILE"),"\\", "Documents\\"))
NCdown <- function(year = NULL, month = NULL, path = NULL) {

  # stringi::stri_escape_unicode("ê")
  # dang::checkPackageAsciiCode(dir = ".")
  # NÃO RETORNAR MENSAGENS DE ERROR
  # options(show.error.messages = FALSE)

  suppressWarnings({

    cli::cli_progress_step("Preparando!", spinner = TRUE)

    if (is.null(year) | is.null(month)) {
      cli::cli_alert_danger(paste0("O 'year' e 'month' devem ser fornecidoS."))
      return(NULL)
    }
    if (year < 2020) {
      cli::cli_alert_danger("O ano deve ser maior ou igual a 2020.")
      return(NULL)
    }
    # if (year == 2020) {
    #   cli::cli_div(theme = list(span.emph = list(color = "orange")))
    #   cli::cli_text("{.emph Desculpa, a extração dO ano de 2020 está em fase de construção.}")
    #   cli::cli_end()
    #   # cli::cli_alert_info("Desculpa, a extração dO ano de 2020 está em fase de construção.")
    #   return(NULL)
    # }
    if (year > timeDate::getRmetricsOptions("currentYear")) {
      # cli::cli_alert_danger("O ano não pode ser maior que o ano atual.")
      return(NULL)
    }

    HtmlLink <- paste0("http://pdet.mte.gov.br/novo-caged/novo-caged-", print(year), "/novo-caged-", print(abjutils::rm_accent(tolower({{month}}))),"-", print(year))


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

      httr::GET(url, httr::write_disk(tf <- paste0(path, "NOVO_CAGED_", month, "_", year, ".xlsx")), httr::progress(), overwrite = TRUE)

      cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos em:", "\n", tf, "\n"))
      # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

      cli::cli_progress_done()

    }
    else {

      HtmlLink2 <- "http://pdet.mte.gov.br/novo-caged"
      HTMLContent2 <- rvest::read_html(HtmlLink2)

      # PEGANDO NO TÍTULO MÊS DA ÚLTIMA PESQUISA
      month1 <- HTMLContent2 %>%
        rvest::html_nodes("[class='outstanding-title']") %>%
        rvest::html_text2() %>%  .[[1]] %>%
        strsplit(., split = " ")  %>%
        unlist() %>%  .[4]


      if(abjutils::rm_accent(tolower({{month}})) == abjutils::rm_accent(tolower({{month1}}))){

        cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

        xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
          rvest::html_elements("a") %>%
          rvest::html_attr("href") %>% .[[3]]

        url <- paste0("http://pdet.mte.gov.br", xlsx2)

        cli::cli_progress_step("Começando o Download dos Dados CAGED", spinner = TRUE)
        cli::cli_progress_step("Download realizado!", spinner = TRUE)

        httr::GET(url, httr::write_disk(tf <- paste0(path, "NOVO_CAGED_", month, "_", year, ".xlsx")), httr::progress(), overwrite = TRUE)

        cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos em:", "\n", tf, "\n"))
        # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

        cli::cli_progress_done()

      }
      if(abjutils::rm_accent(tolower({{month}})) != abjutils::rm_accent(tolower({{month1}})) | is.null(year) & is.null(month) & !is.null(path)){
        cli::cli_alert_info("Possivel erro na declaração das informações de ano e mês! \nVerificar dados informados.\n")
        pesquisa <- gtools::ask(paste("\nUtilizar últiima pesquisa disponnível? (", month1, ") \nInforme 'sim' ou 'não'!"))
        pesquisa <- as.character(pesquisa)

        if(pesquisa == "sim"){
          cli::cli_progress_step("Definindo caminho aos dados", spinner = TRUE)

          xlsx2 <- HTMLContent2 %>% rvest::html_nodes("[class='listaservico span8 module span6']") %>%
            rvest::html_elements("a") %>%
            rvest::html_attr("href") %>% .[[3]]

          url <- paste0("http://pdet.mte.gov.br", xlsx2)

          cli::cli_progress_step("Come\\u00e7ando o Download dos Dados CAGED", spinner = TRUE)
          cli::cli_progress_step("Download realizado!", spinner = TRUE)

          httr::GET(url, httr::write_disk(tf <- paste0(path, "NOVO_CAGED_", month, "_", year, ".xlsx")), httr::progress(), overwrite = TRUE)

          cat(paste("Os dados foram extraidos do link abaixo:", "\n", url, "\nOs arquivos foram salvos em:", "\n", tf, "\n"))
          # cat(paste("\nOs arquivos foram salvos temporariamente em:", "\n", tf))

          cli::cli_progress_done()

        }
        if(pesquisa == "não"){

          cli::cli_alert_danger("Tudo bem, verifique os dados informados!")

        }
      }
    }

  }) #suppressWarnings

}
