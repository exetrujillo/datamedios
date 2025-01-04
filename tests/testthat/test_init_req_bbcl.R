testthat::test_that("Se entrega un data frame de una sola fila, mostrando la noticia más reciente sobre la búsqueda", {

  primer_caso <- init_req_bbcl("inteligencia artificial")

  #Verificamos que es data frame
  expect_s3_class(primer_caso, "data.frame")

  #Verificamos la cantidad de resultados
  expect_equal(nrow(primer_caso), 1)

  #Verificamos la existencia de las columnas deseadas
  expect_true("ID" %in% colnames(primer_caso))
  expect_true("post_title" %in% colnames(primer_caso))
  expect_true("post_content" %in% colnames(primer_caso))
  expect_true("post_excerpt" %in% colnames(primer_caso))
  expect_true("post_URL" %in% colnames(primer_caso))
  expect_true("post_categories" %in% colnames(primer_caso))
  expect_true("post_tags" %in% colnames(primer_caso))
  expect_true("year" %in% colnames(primer_caso))
  expect_true("month" %in% colnames(primer_caso))
  expect_true("day" %in% colnames(primer_caso))
  expect_true("post_category_primary.name" %in% colnames(primer_caso))
  expect_true("post_category_secondary.name" %in% colnames(primer_caso))
  expect_true("post_image.URL" %in% colnames(primer_caso))
  expect_true("post_image.alt" %in% colnames(primer_caso))
  expect_true("post_image.caption" %in% colnames(primer_caso))
  expect_true("author.display_name" %in% colnames(primer_caso))
  expect_true("raw_post_date" %in% colnames(primer_caso))
  expect_true("resumen_de_ia" %in% colnames(primer_caso))









})
