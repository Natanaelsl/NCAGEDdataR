test_that("processar_caged converte Excel em Arrow/Parquet com sucesso", {

  # Caminho absoluto baseado no diretório do projeto
  path_projeto <- rprojroot::find_package_root_file()
  path_mock <- file.path(path_projeto, "inst", "extdata", "caged_miniatura.xlsx")

  # Log de depuração (aparecerá se o teste falhar)
  if (!file.exists(path_mock)) {
    skip(paste("Arquivo não encontrado no local esperado:", path_mock))
  }

  temp_dir <- tempdir()

  # Chamada da função
  res <- processar_caged(
    usar_temporario = FALSE,
    origem = path_mock,
    destino = temp_dir
  )

  # Validações
  expect_false(is.null(res), info = paste("A função retornou NULL para o arquivo:", path_mock))

  arquivos <- list.files(temp_dir, pattern = "\\.parquet$")
  expect_gt(length(arquivos), 0)
})
