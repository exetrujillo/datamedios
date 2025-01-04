library(testthat)

test_that("Error cuando el input no es un data.frame", {
  expect_error(
    datamedios::limpieza_notas(list(a = 1, b = 2)),
    "El argumento 'datos' debe ser un data frame."
  )
})

test_that("Error cuando falta la columna 'post_content'", {
  expect_error(
    datamedios::limpieza_notas(data.frame(otra_columna = c("Texto"))),
    "El data frame debe contener una columna llamada 'post_content'."
  )
})

test_that("Limpieza funciona con datos válidos", {
  datos <- data.frame(
    post_content = c(
      "<div class='lee-tambien-bbcl'>Irrelevante</div><p>Texto relevante</p>",
      "<blockquote class='twitter-tweet'>Tweet</blockquote><p>Más texto</p>"
    ),
    stringsAsFactors = FALSE
  )

  resultado <- datamedios::limpieza_notas(datos)

  expect_equal(nrow(resultado), 2)
  expect_equal(resultado$post_content[1], "Texto relevante")
  expect_equal(resultado$post_content[2], "Más texto")
})

test_that("Sinónimos filtran correctamente", {
  datos <- data.frame(
    post_content = c(
      "<p>Esto es sobre congreso.</p>",
      "<p>Esto no contiene nada.</p>"
    ),
    search_query = "prueba",
    stringsAsFactors = FALSE
  )

  resultado <- datamedios::limpieza_notas(datos, sinonimos = c("congreso"))

  expect_equal(nrow(resultado), 1)
  expect_match(resultado$post_content[1], "congreso")
})

test_that("Errores cuando 'sinonimos' no es un vector de caracteres", {
  datos <- data.frame(post_content = c("Texto de prueba"), stringsAsFactors = FALSE)
  expect_error(
    datamedios::limpieza_notas(datos, sinonimos = 123),
    "'sinonimos' debe ser un vector de palabras."
  )
})

test_that("La función limpia el texto de forma correcta"){
  # Data frame de prueba
  datos <- data.frame(
    post_content = c(
      "<div class='lee-tambien-bbcl'>Irrelevante</div><p>Texto relevante</p>",
      "<blockquote class='twitter-tweet'>Tweet</blockquote><p>Más texto relevante</p>"
    ),
    search_query = "Texto",
    stringsAsFactors = FALSE
  )
}
  # Ejecución de la función
  resultado <- datamedios::limpieza_notas(datos)

  # Verificación de que el texto está limpio
  expect_equal(nrow(resultado), 2) # Ninguna fila eliminada en este caso
  expect_equal(resultado$post_content[1], "Texto relevante")
  expect_equal(resultado$post_content[2], "Más texto relevante")
testthat::test_that("Se recibe un objeto de clase data frame, con 407 resultados y las columans esperadas", {

  resultado <- extraer_noticias_fecha("boric", "2023-01-01", "2023-02-01", subir_a_bd = FALSE)
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
