test_that("Error cuando el input no es un data frame", {
  expect_error(
    tabla_frecuencia_palabras(list(a = 1, b = 2), max_words = 5),
    "'datos' debe ser un data frame."
  )
})

test_that("Error cuando falta la columna 'post_content'", {
  expect_error(
    tabla_frecuencia_palabras(data.frame(otra_columna = c("Texto")), max_words = 5),
    "'datos' debe contener una columna llamada 'post_content'."
  )
})

test_that("Error cuando 'max_words' no es numérico", {
  datos <- data.frame(post_content = c("Texto relevante."))
  expect_error(
    tabla_frecuencia_palabras(datos, max_words = "cinco"),
    "'max_words' debe ser un número."
  )
})

test_that("Error cuando 'stop_words' no es un vector de caracteres", {
  datos <- data.frame(post_content = c("Texto relevante."))
  expect_error(
    tabla_frecuencia_palabras(datos, max_words = 5, stop_words = 123),
    "'stop_words' debe ser un vector de palabras."
  )
})

test_that("Tabla de palabras frecuentes funciona correctamente con datos válidos", {
  # Datos de prueba usando la función `extraer_noticias_max_res`
  datos <- extraer_noticias_max_res("cambio climático", max_results = 10, subir_a_bd = FALSE)
  datos <- limpieza_notas(datos)

  # Generar la tabla
  tabla <- tabla_frecuencia_palabras(datos, max_words = 5)

  # Verificar que la tabla generada es un objeto de clase datatable
  expect_s3_class(tabla, "datatables")

  # Verificar que las palabras y frecuencias sean correctas
  palabras_frecuentes <- datos %>%
    tidytext::unnest_tokens(word, post_content) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::slice_max(n, n = 5)

  # Comparar las palabras en la tabla generada
  expect_equal(
    palabras_frecuentes$word,
    tabla$x$data[[2]], # Primera columna de la tabla generada
    info = "Las palabras frecuentes no coinciden."
  )
})

test_that("Stop words son excluidas correctamente", {
  datos <- data.frame(post_content = c("Esto es un ejemplo. Esto es una prueba."))

  # Generar la tabla excluyendo palabras comunes
  tabla <- tabla_frecuencia_palabras(datos, max_words = 3, stop_words = c("esto", "es", "un", "una"))

  # Verificar que las palabras stop fueron excluidas
  palabras_en_tabla <- tabla$x$data[[1]] # Primera columna de la tabla generada
  expect_false(any(palabras_en_tabla %in% c("esto", "es", "un", "una")))
})

