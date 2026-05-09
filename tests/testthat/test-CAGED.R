test_that("CAGED() interrompe execução se não encontrar arquivos", {
  # Como a função agora usa stop(), o teste deve esperar um erro
  expect_error(CAGED(arquivo_alvo = "ano_que_nao_existe"))
})
