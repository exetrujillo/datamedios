testthat::test_that("Se recibe un objeto de clase data frame, con 201 resultados y las columans esperadas", {

  resultado <- extraer_noticias_max_res("boric", max_results = 201)

  #Verificamos que es data frame
  expect_s3_class(resultado, "data.frame")

  #Verificamos la cantidad de resultados
  expect_equal(nrow(resultado), 201)

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

})


