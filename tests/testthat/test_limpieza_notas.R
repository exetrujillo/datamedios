testthat::test_that("Se recibe un objeto de clase data frame, con 407 resultados y las columans esperadas", {

  resultado <- extraer_noticias_fecha("boric", "2023-01-01", "2023-02-01")
  resultado <- limpieza_notas(resultado, c("IA", "AI"))

  #Verificamos que es data frame
  expect_s3_class(resultado, "data.frame")

  #Verificamos la cantidad de resultados
  expect_equal(nrow(resultado), 407)

  # Verificamos que el texto no tenga html (corchetes <>)
  indice_aleatorio <- sample(1:407, 1)
  expect_false(grepl("<[^>]+>", resultado$post_content[indice_aleatorio]),
               info = paste("Prueba falló en el índice", indice_aleatorio))

  #Verificamos la existencia de las columnas deseadas
  expect_true("ID" %in% colnames(resultado))
  expect_true("post_title" %in% colnames(resultado))
  expect_true("post_content" %in% colnames(resultado))
  expect_true("post_excerpt" %in% colnames(resultado))
  expect_true("post_URL" %in% colnames(resultado))
  expect_true("post_categories" %in% colnames(resultado))
  expect_true("post_tags" %in% colnames(resultado))
  expect_true("year" %in% colnames(resultado))
  expect_true("month" %in% colnames(resultado))
  expect_true("day" %in% colnames(resultado))
  expect_true("post_category_primary.name" %in% colnames(resultado))
  expect_true("post_category_secondary.name" %in% colnames(resultado))
  expect_true("post_image.URL" %in% colnames(resultado))
  expect_true("post_image.alt" %in% colnames(resultado))
  expect_true("post_image.caption" %in% colnames(resultado))
  expect_true("author.display_name" %in% colnames(resultado))
  expect_true("raw_post_date" %in% colnames(resultado))
  expect_true("resumen_de_ia" %in% colnames(resultado))
  expect_true("search_query" %in% colnames(resultado))

})
