library(testthat)

# Prueba para la extraccion de noticias de Ciper
test_that("extraer_noticias_max_res_ciper funciona correctamente", {
  # Evitar hacer llamadas reales a la API en tests automaticos
  skip_on_cran()

  # Termino de busqueda comun para asegurar resultados
  search_query <- "derechos humanos"
  max_results <- NULL

  # Ejecutar la funcion
  noticias <- extraer_noticias_max_res(search_query, max_results = 3, fuentes = "ciper", subir_a_bd = FALSE)

  # 1. Verificar que el resultado es un dataframe
  expect_s3_class(noticias, "data.frame")

  # 2. Verificar que el dataframe no este vacio
  expect_true(nrow(noticias) > 0)

  # 3. Verificar que el numero de filas es el esperado
  expect_equal(nrow(noticias), max_results)

  # 4. Verificar que las columnas esenciales esten presentes
  columnas_esperadas <- c("ID", "titulo", "contenido", "contenido_limpio", "url", "fecha", "resumen", "search_query", "medio")
  expect_true(all(columnas_esperadas %in% names(noticias)))

  # 5. Verificar que el medio es 'ciper'
  expect_true(all(noticias$medio == "ciper"))

  # 6. Verificar que la columna de contenido limpio no este vacia
  expect_true(all(!is.na(noticias$contenido_limpio)))
  expect_true(all(nchar(noticias$contenido_limpio) > 0))

})
