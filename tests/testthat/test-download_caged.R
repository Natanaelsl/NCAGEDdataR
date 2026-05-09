test_that("download_caged valida parâmetros de entrada", {
  # Verifica se a função retorna NULL ou erro para anos inválidos (ex: antes de 2020)
  expect_null(download_caged(ref = "1999"))
})
