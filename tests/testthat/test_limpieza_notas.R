# Test para validar errores por entradas incorrectas
test_that("Error cuando el input no es un data.frame", {
  expect_error(
    datamedios::limpieza_notas(list(a = 1, b = 2)),
    "El argumento 'datos' debe ser un data frame."
  )
})

test_that("Error cuando falta la columna 'contenido'", {
  expect_error(
    datamedios::limpieza_notas(data.frame(otra_columna = c("Texto"))),
    "El data frame debe contener una columna llamada 'contenido'."
  )
})

test_that("Errores cuando 'sinonimos' no es un vector de caracteres", {
  datos <- data.frame(contenido = c("Texto de prueba"), stringsAsFactors = FALSE)
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

  # Verificamos que solo queden filas relevantes
  expect_true(
    any(grepl("presidente", resultado$contenido) & !grepl("boric", resultado$contenido)),
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

  es_html <- tryCatch({
    xml2::read_html(resultado$contenido_limpio[indice_aleatorio])
    TRUE
  }, error = function(e) {
    print(paste("Error al procesar HTML en la fila:", indice_aleatorio))
    print(resultado$contenido_limpio[indice_aleatorio])
    print(paste("Mensaje de error:", e$message))
    FALSE
  })

  expect_false(
    es_html,
    info = paste("HTML no fue eliminado correctamente en la fila", indice_aleatorio)
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
    "ID", "titulo", "contenido", "contenido_limpio", "url", "url_imagen",
    "autor", "fecha", "resumen", "search_query", "medio",
    "temas"
  )
  for (columna in columnas_esperadas) {
    expect_true(
      columna %in% colnames(resultado),
      info = paste("La columna", columna, "no está en el resultado.")
    )
  }

  # Verificamos que no quede HTML en una fila aleatoria
  indice_aleatorio <- sample(1:nrow(resultado), 1)

  es_html <- tryCatch({
    xml2::read_html(resultado$contenido_limpio[indice_aleatorio])
    TRUE
  }, error = function(e) {
    print(paste("Error al procesar HTML en la fila:", indice_aleatorio))
    print(resultado$contenido_limpio[indice_aleatorio])
    print(paste("Mensaje de error:", e$message))
    FALSE
  })

  expect_false(
    es_html,
    info = paste("HTML no fue eliminado correctamente en la fila", indice_aleatorio)
  )
})
