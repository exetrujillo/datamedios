library(testthat)

# Test para validar errores por entradas incorrectas
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

test_that("Errores cuando 'sinonimos' no es un vector de caracteres", {
  datos <- data.frame(post_content = c("Texto de prueba"), stringsAsFactors = FALSE)
  expect_error(
    datamedios::limpieza_notas(datos, sinonimos = 123),
    "'sinonimos' debe ser un vector de palabras."
  )
})

# Test para verificar filtrado por sinónimos
test_that("Se realiza el filtrado correctamente considerando los sinónimos", {
  # Datos generados con la función del paquete
  datos <- datamedios::extraer_noticias_max_res("boric", max_results = 100, subir_a_bd = FALSE)

  resultado <- datamedios::limpieza_notas(datos, sinonimos = c("presidente"))

  #

  # Verificamos que solo queden filas relevantes
expect_true(
  any(grepl("presidente", resultado$post_content) & !grepl("boric", resultado$post_content)),
  info = "No se encontró ninguna fila donde aparezca 'presidente' pero no 'boric'."
)
})

# Test para limpiar datos extraídos con la función de tu paquete
test_that("La función limpia el texto de forma correcta en datos reales", {
  # Datos generados con la función del paquete
  datos <- datamedios::extraer_noticias_max_res("boric", max_results = 100, subir_a_bd = FALSE)

  # Ejecución de la función de limpieza
  resultado <- datamedios::limpieza_notas(datos)

  # Verificamos que no quede HTML en una fila aleatoria
  indice_aleatorio <- sample(1:nrow(resultado), 1)
  expect_false(
    grepl("<[^>]+>", resultado$post_content[indice_aleatorio]),
    info = paste("HTML no fue eliminado en la fila", indice_aleatorio)
  )
})

# Test final para verificar estructura, limpieza y columnas esperadas
test_that("Resultados finales tienen estructura correcta y columnas esperadas", {
  # Extracción inicial
  datos <- datamedios::extraer_noticias_max_res("boric", max_results = 10, subir_a_bd = FALSE)
  resultado <- datamedios::limpieza_notas(datos, c("presidente"))

  # Verificamos que el resultado es un data.frame
  expect_s3_class(resultado, "data.frame")

  # Verificamos que las columnas deseadas existen
  columnas_esperadas <- c(
    "ID", "post_title", "post_content", "post_excerpt", "post_URL",
    "post_categories", "post_tags", "year", "month", "day",
    "post_category_primary.name", "post_category_secondary.name",
    "post_image.URL", "post_image.alt", "post_image.caption",
    "author.display_name", "raw_post_date", "resumen_de_ia", "search_query"
  )
  for (columna in columnas_esperadas) {
    expect_true(
      columna %in% colnames(resultado),
      info = paste("La columna", columna, "no está en el resultado.")
    )
  }

  # Verificamos que no quede HTML en una fila aleatoria
  indice_aleatorio <- sample(1:nrow(resultado), 1)
  expect_false(
    grepl("<[^>]+>", resultado$post_content[indice_aleatorio]),
    info = paste("HTML no fue eliminado en la fila", indice_aleatorio)
  )
})
