test_that("processar_caged converte Excel em Arrow/Parquet com sucesso", {
  path_mock <- system.file("extdata", "caged_miniatura.xlsx", package = "NCAGEDdataR")

  # Fallback para desenvolvimento local
  if (path_mock == "") path_mock <- "../../inst/extdata/caged_miniatura.xlsx"

  if (!file.exists(path_mock)) skip("Mock não encontrado.")

  temp_dir <- tempdir()
  res <- processar_caged(usar_temporario = FALSE, origem = path_mock, destino = temp_dir)
  expect_false(is.null(res))
})
