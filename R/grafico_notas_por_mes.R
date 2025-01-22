#' Grafico de notas por mes
#'
#' Esta funcion genera un grafico de linea que muestra la cantidad de publicaciones agrupadas por mes.
#'
#' @param datos Data frame con los datos procesados, que debe incluir la columna `fecha` en formato YYYY-MM-DD.
#' @param titulo Texto que aparecera en el titulo del grafico.
#' @param fecha_inicio Fecha de inicio para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param fecha_fin Fecha de finalizacion para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @return Un grafico ggplot2 que muestra la cantidad de publicaciones por mes.
#' @examples
#' \dontrun{
#' datos <- extraer_noticias_fecha("cambio climatico", "2024-01-01","2025-01-01", subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos)
#' datos_proc <- limpieza_notas(datos)
#' grafico_notas_por_mes(datos_proc, titulo = "Cambio Climatico",
#' fecha_inicio = "2024-01-01", fecha_fin = "2025-01-01")
#' grafico_notas_por_mes(datos_proc, titulo = "Cambio Climatico")
#' }
#'
#' @export

grafico_notas_por_mes <- function(datos, titulo, fecha_inicio = NULL, fecha_fin = NULL) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'fecha' exista en los datos
  if (!"fecha" %in% colnames(datos)) {
    stop("'datos' debe contener la columna 'fecha'.")
  }

  # Validar que 'titulo' sea texto
  if (!is.character(titulo)) {
    stop("'titulo' debe ser texto (string).")
  }

  # Convertir 'fecha' a formato fecha
  datos <- datos %>%
    dplyr::mutate(fecha = as.Date(fecha, format = "%Y-%m-%d"))

  # Filtrar por rango de fechas si se especifican
  if (!is.null(fecha_inicio) || !is.null(fecha_fin)) {
    if (!is.null(fecha_inicio)) {
      fecha_inicio <- as.Date(fecha_inicio, format = "%Y-%m-%d")
    } else {
      fecha_inicio <- min(datos$fecha, na.rm = TRUE)
    }
    if (!is.null(fecha_fin)) {
      fecha_fin <- as.Date(fecha_fin, format = "%Y-%m-%d")
    } else {
      fecha_fin <- max(datos$fecha, na.rm = TRUE)
    }

    # Validar que las fechas sean validas
    if (fecha_inicio > fecha_fin) {
      stop("'fecha_inicio' debe ser anterior o igual a 'fecha_fin'.")
    }

    # Filtrar datos por rango
    datos_filtrados <- datos %>%
      dplyr::filter(fecha >= fecha_inicio & fecha <= fecha_fin)
  } else {
    # Usar todo el rango de fechas disponible
    datos_filtrados <- datos
  }

  # Verificar si hay datos despues del filtrado
  if (nrow(datos_filtrados) == 0) {
    stop("No hay datos en el rango de fechas seleccionado.")
  }

  # Agrupar datos por mes y contar las publicaciones
  publicaciones_por_mes <- datos_filtrados %>%
    dplyr::mutate(fecha = lubridate::floor_date(fecha, "month")) %>%
    dplyr::group_by(fecha) %>%
    dplyr::summarise(cantidad = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(fecha)

  # Generar el grafico
  grafico <- ggplot2::ggplot(publicaciones_por_mes, ggplot2::aes(x = fecha, y = cantidad)) +
    ggplot2::geom_line(color = "blue", linewidth = 1) +
    ggplot2::geom_point(color = "red", size = 1) +
    ggplot2::geom_smooth(method = "loess", color = "green", se = FALSE, linewidth = 1) +
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = "Cantidad de Notas"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Devolver el grafico
  return(grafico)
}
