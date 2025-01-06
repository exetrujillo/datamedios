test_that("La función procesa correctamente los datos sin sinónimos", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c(
      "<html><p>La inteligencia artificial está cambiando el mundo.</p></html>",
      "<html><p>No hay menciones aquí.</p></html>"
    ),
    stringsAsFactors = FALSE
  )
  resultado <- extraccion_parrafos(datos)
  expect_true("parrafos_filtrados" %in% colnames(resultado))
  expect_equal(resultado$parrafos_filtrados[[1]], "La inteligencia artificial está cambiando el mundo.")
  expect_equal(resultado$parrafos_filtrados[[2]], character(0))
})

test_that("La función procesa correctamente los datos con sinónimos", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c(
      "<html><p>La IA está en todas partes.</p><p>La inteligencia artificial está en auge.</p></html>",
      "<html><p>No hay menciones aquí.</p></html>"
    ),
    stringsAsFactors = FALSE
  )
  sinonimos <- c("IA", "AI")
  resultado <- extraccion_parrafos(datos, sinonimos)
  expect_equal(resultado$parrafos_filtrados[[1]], c("La IA está en todas partes.", "La inteligencia artificial está en auge."))
  expect_equal(resultado$parrafos_filtrados[[2]], character(0))
})

test_that("La función lanza un error si falta la columna 'post_content'", {
  datos <- data.frame(search_query = "inteligencia artificial")
  expect_error(extraccion_parrafos(datos), "El data frame debe contener la columna 'post_content'.")
})

test_that("La función lanza un error si 'datos' no es un data frame", {
  datos <- list(search_query = "inteligencia artificial", post_content = "Contenido")
  expect_error(extraccion_parrafos(datos), "'datos' debe ser un data frame.")
})

test_that("La función maneja contenido HTML mal formado", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c("<html><p>Texto válido</p>", "Contenido no HTML"),
    stringsAsFactors = FALSE
  )
  resultado <- extraccion_parrafos(datos)
  expect_true(all(is.na(resultado$parrafos_filtrados[[2]])))
})

test_that("La función maneja correctamente una columna 'post_content' vacía", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c("", ""),
    stringsAsFactors = FALSE
  )
  resultado <- extraccion_parrafos(datos)
  expect_true(all(sapply(resultado$parrafos_filtrados, is.na)))
})

test_that("La función procesa correctamente cuando 'sinonimos' está vacío", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c("<html><p>Texto sobre inteligencia artificial.</p></html>"),
    stringsAsFactors = FALSE
  )
  resultado <- extraccion_parrafos(datos, sinonimos = NULL)
  expect_equal(resultado$parrafos_filtrados[[1]], "Texto sobre inteligencia artificial.")
})

test_that("La función procesa textos largos con párrafos mixtos", {
  datos <- data.frame(
    search_query = "inteligencia artificial",
    post_content = c(
      "<html><p>La IA es una revolución.</p><p>Este es un párrafo irrelevante.</p><p>La inteligencia artificial cambiará el mundo.</p></html>"
    ),
    stringsAsFactors = FALSE
  )
  sinonimos <- c("IA", "AI")
  resultado <- extraccion_parrafos(datos, sinonimos)
  expect_equal(
    resultado$parrafos_filtrados[[1]],
    c("La IA es una revolución.", "La inteligencia artificial cambiará el mundo.")
  )
})

test_that("La función elimina correctamente el HTML en los párrafos extraídos", {
  # Generar datos simulados usando extraer_noticias_max_res
  datos <- extraer_noticias_max_res("boric", max_results = 20, subir_a_bd = FALSE)

  # Sinónimos para la búsqueda
  sinonimos <- c("IA", "AI")

  # Procesar los datos con extraccion_parrafos
  resultado <- extraccion_parrafos(datos, sinonimos)

  # Comprobar que no exista HTML en los párrafos extraídos
  contiene_html <- any(grepl("<[^>]+>", unlist(resultado$parrafos_filtrados), perl = TRUE))

  # Evaluar el test
  expect_false(contiene_html, info = "Los párrafos procesados no deberían contener HTML.")
})


