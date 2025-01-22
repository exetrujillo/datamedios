testthat::test_that("Se recibe un objeto de clase data frame, con 470 resultados y las columnas esperadas", {

  resultado <- extraer_noticias_fecha("boric", "2023-01-01", "2023-02-01", subir_a_bd = FALSE)

  #Verificamos que es data frame
  expect_s3_class(resultado, "data.frame")

  #Verificamos la cantidad de resultados
  expect_equal(nrow(resultado), 470) # No debería cambiar la cantidad de notas ya publicadas de esa fecha

  #Verificamos la existencia de las columnas deseadas
  expect_true("ID" %in% colnames(resultado))
  expect_true("titulo" %in% colnames(resultado))
  expect_true("contenido" %in% colnames(resultado))
  expect_true("contenido_limpio" %in% colnames(resultado))
  expect_true("url" %in% colnames(resultado))
  expect_true("url_imagen" %in% colnames(resultado))
  expect_true("autor" %in% colnames(resultado))
  expect_true("fecha" %in% colnames(resultado))
  expect_true("resumen" %in% colnames(resultado))
  expect_true("search_query" %in% colnames(resultado))
  expect_true("medio" %in% colnames(resultado))
  expect_true("temas" %in% colnames(resultado))

})
