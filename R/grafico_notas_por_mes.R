#' Gráfico de publicaciones por mes
#'
#' Esta función genera un gráfico de línea que muestra la cantidad de publicaciones agrupadas por mes, utilizando un título dinámico basado en la consulta de búsqueda.
#'
#' @param datos Data frame con los datos procesados, que debe incluir las columnas `year` y `month`.
#' @param titulo Texto que aparecerá en el título del gráfico.
#' @return Un gráfico ggplot2 que muestra la cantidad de publicaciones por mes.
#' @examples
#' grafico_publicaciones_por_mes(datos_proc, search_query = "Cambio Climático")
#' @export

grafico_publicaciones_por_mes <- function(datos, search_query) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'year' y 'month' existan en los datos
  if (!all(c("year", "month") %in% colnames(datos))) {
    stop("'datos' debe contener las columnas 'year' y 'month'.")
  }

  # Validar que 'search_query' sea un texto
  if (!is.character(search_query) || length(search_query) != 1) {
    stop("'search_query' debe ser un texto (string) de longitud 1.")
  }

  # Agrupar datos por año y mes, contar las publicaciones y crear la fecha para el eje x
  publicaciones_por_mes <- datos %>%
    group_by(year, month) %>%
    summarise(cantidad = n(), .groups = 'drop') %>%
    mutate(fecha = as.Date(paste(year, month, "01", sep = "-")))

  # Crear título dinámico basado en search_query
  titulo_grafico <- paste("Cantidad de Notas sobre", search_query, "publicadas por mes")

  # Generar el gráfico
  grafico <- ggplot(publicaciones_por_mes, aes(x = fecha, y = cantidad)) +
    geom_line(color = "blue", linewidth = 1) +  # Línea de publicaciones
    geom_point(color = "red", size = 1) +  # Puntos en cada mes
    geom_smooth(method = "loess", color = "green", se = FALSE, linewidth = 1) +  # Curva de tendencia
    labs(
      title = titulo_grafico,  # Título dinámico
      x = "Año",
      y = "Cantidad de Notas"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")  # Mostrar solo los años en el eje x

  # Devolver el gráfico
  return(grafico)
}
