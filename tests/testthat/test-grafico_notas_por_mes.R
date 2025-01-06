test_that("La función lanza un error si 'datos' no es un data frame", {
  expect_error(
    grafico_notas_por_mes("no es un data frame", "Título"),
    "'datos' debe ser un data frame."
  )
})

test_that("La función lanza un error si 'raw_post_date' no está en los datos", {
  datos_sin_fecha <- data.frame(otra_columna = c(1, 2, 3))
  expect_error(
    grafico_notas_por_mes(datos_sin_fecha, "Título"),
    "'datos' debe contener la columna 'raw_post_date'."
  )
})

test_that("La función lanza un error si 'titulo' no es texto", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01"))
  expect_error(
    grafico_notas_por_mes(datos, 123),
    regexp = "'titulo' debe ser texto \\(string\\)."
  )
})

test_that("La función lanza un error si 'fecha_inicio' es posterior a 'fecha_fin'", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01"))
  expect_error(
    grafico_notas_por_mes(datos, "Título", fecha_inicio = "2024-03-01", fecha_fin = "2024-01-01"),
    "'fecha_inicio' debe ser anterior o igual a 'fecha_fin'."
  )
})

test_that("La función lanza un error si no hay datos en el rango de fechas", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01"))
  expect_error(
    grafico_notas_por_mes(datos, "Título", fecha_inicio = "2025-01-01", fecha_fin = "2025-12-31"),
    "No hay datos en el rango de fechas seleccionado."
  )
})

test_that("La función genera un gráfico correctamente con datos válidos", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01", "2024-02-15", "2024-03-01"))
  grafico <- grafico_notas_por_mes(datos, "Título")
  expect_s3_class(grafico, "ggplot")
})

test_that("La función filtra correctamente por rango de fechas", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01", "2024-02-15", "2024-03-01"))
  grafico <- grafico_notas_por_mes(datos, "Título", fecha_inicio = "2024-02-01", fecha_fin = "2024-02-28")
  expect_s3_class(grafico, "ggplot")
})

test_that("Las fechas en el gráfico están dentro del rango especificado", {
  datos <- data.frame(raw_post_date = c("2024-01-01", "2024-02-01", "2024-02-15", "2024-03-01", "2024-04-01"))

  # Generar gráfico con un rango de fechas específico
  grafico <- grafico_notas_por_mes(datos, "Título", fecha_inicio = "2024-02-01", fecha_fin = "2024-03-31")

  # Extraer las fechas del objeto gráfico
  fechas_grafico <- grafico$data$fecha

  # Comprobar que todas las fechas están dentro del rango
  expect_true(all(fechas_grafico >= as.Date("2024-02-01") & fechas_grafico <= as.Date("2024-03-31")))
})
