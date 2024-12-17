#' Gráfico de notas por mes
#'
#' Esta función genera un gráfico de línea que muestra la cantidad de publicaciones agrupadas por mes, utilizando un título dinámico basado en la consulta de búsqueda.
#'
#' @param datos Data frame con los datos procesados, que debe incluir las columnas `year` y `month`.
#' @param titulo Texto que aparecerá en el título del gráfico.
#' @param year_inicio Año de inicio para la construcción del gráfico
#' @param month_inicio Mes de inciio para la construcción del gráfico
#' @param year_fin Año de finalización para la construcción del gráfico
#' @param month_fin Mes de finalización para la construcción del gráfico
#' @return Un gráfico ggplot2 que muestra la cantidad de publicaciones por mes.
#' @examples
#' grafico_notas_por_mes(datos_proc, titulo = "Cambio Climático")
#' @export

grafico_notas_por_mes <- function(datos, titulo, year_inicio = NULL, month_inicio = NULL, year_fin = NULL, month_fin = NULL) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'year' y 'month' existan en los datos
  if (!all(c("year", "month") %in% colnames(datos))) {
    stop("'datos' debe contener las columnas 'year' y 'month'.")
  }

  # Validar que 'titulo' sea un texto
  if (!is.character(titulo)) {
    stop("'titulo' debe ser texto (string).")
  }

  # Si no se especifica un rango de fechas, usar todo el rango de los datos
  if (is.null(year_inicio)) {
    year_inicio <- min(datos$year)
    month_inicio <- min(datos$month)
  }

  if (is.null(year_fin)) {
    year_fin <- max(datos$year)
    month_fin <- max(datos$month)
  }

  # Validar que los parámetros de año y mes sean numéricos
  if (!is.numeric(year_inicio) | !is.numeric(year_fin) |
      !is.numeric(month_inicio) | !is.numeric(month_fin)) {
    stop("Los parámetros 'year_inicio', 'month_inicio', 'year_fin' y 'month_fin' deben ser numéricos.")
  }

  # Validar rango de tiempo
  if (year_inicio > year_fin | (year_inicio == year_fin & month_inicio > month_fin)) {
    stop("'year_inicio' y 'month_inicio' deben ser anteriores o iguales a 'year_fin' y 'month_fin'.")
  }

  # Filtrar datos dentro del rango especificado
  datos_filtrados <- datos %>%
    dplyr::filter(
      (datos$year > year_inicio | (datos$year == year_inicio & datos$month >= month_inicio)) &
        (datos$year < year_fin | (datos$year == year_fin & datos$month <= month_fin))
    )

  # Verificar si hay datos después del filtrado
  if (nrow(datos_filtrados) == 0) {
    stop("No hay datos en el rango de fechas seleccionado.")
  }

  # Agrupar datos por año y mes, contar las publicaciones
  publicaciones_por_mes <- datos_filtrados %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(cantidad = n(), .groups = 'drop') %>%
    dplyr::arrange(year, month)  # Ordenar por año y mes

  # Generar el gráfico
  grafico <- ggplot2::ggplot(publicaciones_por_mes, aes(x = interaction(year, month, sep = "-"), y = cantidad)) +
    geom_line(group = 1, color = "blue", linewidth = 1) +  # Línea de publicaciones
    geom_point(color = "red", size = 1) +  # Puntos en cada mes
    geom_smooth(method = "loess", color = "green", se = FALSE, linewidth = 1) +  # Curva de tendencia
    labs(
      title = titulo,
      x = "Año-Mes",
      y = "Cantidad de Notas"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x

  # Devolver el gráfico
  return(grafico)
}
