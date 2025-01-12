test_that("Error cuando el input no es un data frame", {
  expect_error(
    word_cloud(list(a = 1, b = 2), max_words = 50),
    "'datos' debe ser un data frame."
  )
})

test_that("Error cuando falta la columna 'post_content_clean'", {
  expect_error(
    word_cloud(data.frame(otra_columna = c("Texto")), max_words = 50),
    "'datos' debe contener una columna llamada 'post_content_clean'."
  )
})

test_that("Error cuando 'max_words' no es numérico", {
  datos <- data.frame(post_content_clean = c("Texto relevante."))
  expect_error(
    word_cloud(datos, max_words = "cincuenta"),
    "'max_words' debe ser un numero."
  )
})

test_that("Error cuando 'stop_words' no es un vector de caracteres", {
  datos <- data.frame(post_content_clean = c("Texto relevante."))
  expect_error(
    word_cloud(datos, max_words = 50, stop_words = 123),
    "'stop_words' debe ser un vector de palabras."
  )
})

test_that("La función genera una nube de palabras correctamente", {
  datos <- data.frame(
    post_content_clean = c(
      "La inteligencia artificial está transformando el mundo.",
      "La transformación digital depende de la inteligencia artificial."
    )
  )

  # Generar la nube de palabras
  nube <- word_cloud(datos, max_words = 5)

  # Verificar que se genera un objeto de clase wordcloud2
  expect_s3_class(nube, "wordcloud2")

  # Verificar las palabras más frecuentes
  palabras_frecuentes <- datos %>%
    tidytext::unnest_tokens(word, post_content_clean) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::slice_max(n, n = 5)

  # Asegurarse de que las palabras aparecen en la nube
  palabras_en_nube <- nube$x$word
  expect_true(all(palabras_frecuentes$word %in% palabras_en_nube))
})

test_that("Stop words son excluidas correctamente", {
  datos <- data.frame(post_content_clean = c("Esto es un ejemplo. Esto es una prueba."))

  # Generar la nube de palabras excluyendo stop words
  nube <- word_cloud(datos, max_words = 3, stop_words = c("esto", "es", "un", "una"))

  # Verificar que las palabras stop no están en la nube
  palabras_en_nube <- nube$x$word
  expect_false(any(palabras_en_nube %in% c("esto", "es", "un", "una")))
})

test_that("La función maneja casos límite de datos correctamente", {
  datos <- data.frame(post_content_clean = c(""))

  # Generar la nube con un texto vacío
  expect_error(
    word_cloud(datos, max_words = 5),
    "No se encontraron palabras para generar la nube."
  )

  # Generar la nube con un único dato
  datos <- data.frame(post_content_clean = c("inteligencia artificial"))
  nube <- word_cloud(datos, max_words = 5)

  # Verificar que la palabra aparece en la nube
  palabras_en_nube <- nube$x$word
  expect_true("inteligencia" %in% palabras_en_nube)
  expect_true("artificial" %in% palabras_en_nube)
})

test_that("'max_words' limita correctamente la cantidad de palabras en la nube", {
  datos <- data.frame(
    post_content_clean = c(
      "La inteligencia artificial está transformando el mundo.",
      "La transformación digital depende de la inteligencia artificial.",
      "El futuro está marcado por la inteligencia artificial y la digitalización."
    )
  )

  # Generar la nube con max_words = 3
  nube <- word_cloud(datos, max_words = 3)

  # Obtener las palabras en la nube
  palabras_en_nube <- nube$x$word

  # Verificar que el número de palabras no excede max_words
  expect_lte(length(palabras_en_nube), 3)
})
