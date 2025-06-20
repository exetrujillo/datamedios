#' Grafico de notas por periodo
#'
#' Esta funcion genera un grafico que muestra la cantidad de publicaciones
#' agrupadas por dia o por mes.
#'
#' @param datos Data frame con los datos procesados, que debe incluir la columna `fecha` en formato YYYY-MM-DD.
#' @param titulo Texto que aparecera en el titulo del grafico.
#' @param fecha_inicio Fecha de inicio para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param fecha_fin Fecha de finalizacion para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param agrupar_por Cadena de texto que especifica el periodo de agrupacion.
#'   Valores validos son `"day"` (por defecto) o `"month"`.
#' @return Un grafico ggplot2 que muestra la cantidad de publicaciones por el periodo seleccionado.
#' @examples
#' \dontrun{
#' # Ejemplo original, agrupando por mes
#' datos <- extraer_noticias_fecha("cambio climatico", "2024-01-01","2025-01-01", subir_a_bd = FALSE)
#' grafico_notas_por_mes(datos, titulo = "Cambio Climatico (por mes)",
#'                       agrupar_por = "month")
#' # Devuelve un grafico de barras con la cantidad de notas por mes
#'
#' # Nuevo ejemplo, agrupando por dia para mayor granularidad
#' grafico_notas_por_mes(datos, titulo = "Cambio Climatico (por dia)",
#'                       fecha_inicio = "2024-01-01", fecha_fin = "2024-03-31")
#' # Devuelve un grafico de barras con la cantidad de notas por dia
#' }
#'
#' @export
grafico_notas_fecha <- function(datos, titulo, fecha_inicio = NULL, fecha_fin = NULL, agrupar_por = "day") {
  # --- 1. Validacion de Entradas ---

  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }
  # Validar que 'fecha' exista en los datos
  if (!"fecha" %in% colnames(datos)) {
    stop("'datos' debe contener la columna 'fecha'.")
  }
  # Validar que 'titulo' sea texto
  if (!is.character(titulo) || length(titulo) != 1) {
    stop("'titulo' debe ser una unica cadena de texto (string).")
  }
  # Validar el nuevo parametro 'agrupar_por'
  if (!agrupar_por %in% c("month", "day")) {
    stop("'agrupar_por' debe ser 'month' o 'day'.")
  }

  # --- 2. Preparacion y Filtrado de Datos ---

  # Convertir 'fecha' a formato fecha y asegurar que no hay horas/minutos
  datos_proc <- datos %>%
    dplyr::mutate(fecha = as.Date(fecha, format = "%Y-%m-%d"))

  # Filtrar por rango de fechas si se especifican
  if (!is.null(fecha_inicio) || !is.null(fecha_fin)) {
    if (!is.null(fecha_inicio)) {
      fecha_inicio <- as.Date(fecha_inicio, format = "%Y-%m-%d")
    } else {
      fecha_inicio <- min(datos_proc$fecha, na.rm = TRUE)
    }
    if (!is.null(fecha_fin)) {
      fecha_fin <- as.Date(fecha_fin, format = "%Y-%m-%d")
    } else {
      fecha_fin <- max(datos_proc$fecha, na.rm = TRUE)
    }

    # Validar que las fechas sean validas
    if (is.na(fecha_inicio) || is.na(fecha_fin)) {
      stop("Las fechas de inicio/fin no son validas o no hay fechas en los datos.")
    }
    if (fecha_inicio > fecha_fin) {
      stop("'fecha_inicio' debe ser anterior o igual a 'fecha_fin'.")
    }

    # Filtrar datos por rango
    datos_filtrados <- datos_proc %>%
      dplyr::filter(fecha >= fecha_inicio & fecha <= fecha_fin)
  } else {
    # Usar todo el rango de fechas disponible
    datos_filtrados <- datos_proc
  }

  # Verificar si hay datos despues del filtrado
  if (nrow(datos_filtrados) == 0) {
    stop("No hay datos en el rango de fechas seleccionado.")
  }

  # --- 3. Agregacion de Datos Condicional ---

  if (agrupar_por == "month") {
    # Agrupar datos por mes y contar las publicaciones
    publicaciones_agrupadas <- datos_filtrados %>%
      dplyr::mutate(fecha_grupo = lubridate::floor_date(fecha, "month")) %>%
      dplyr::group_by(fecha_grupo) %>%
      dplyr::summarise(cantidad = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(fecha_grupo)

  } else { # agrupar_por == "day"
    # Agrupar datos por dia y contar las publicaciones
    publicaciones_agrupadas <- datos_filtrados %>%
      dplyr::group_by(fecha) %>%
      dplyr::rename(fecha_grupo = fecha) %>%
      dplyr::summarise(cantidad = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(fecha_grupo)
  }


  # --- 4. Generacion del Grafico Condicional ---

  # Definir la base del grafico
  grafico_base <- ggplot2::ggplot(publicaciones_agrupadas, ggplot2::aes(x = fecha_grupo, y = cantidad)) +
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = "Cantidad de Notas"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Añadir las capas (geoms) segun el tipo de agrupacion
  if (agrupar_por == "month") {
    grafico <- grafico_base +
      ggplot2::geom_line(color = "blue", linewidth = 1) +
      ggplot2::geom_point(color = "red", size = 1.5) + # Aumente un poco el tamaño para mejor visibilidad
      ggplot2::geom_smooth(method = "loess", color = "green", se = FALSE, linewidth = 1)
  } else { # agrupar_por == "day"
    grafico <- grafico_base +
      ggplot2::geom_col(fill = "blue", color = "blue", width = 1) # Usamos geom_col para barras
  }

  # --- 5. Devolver el Grafico ---
  return(grafico)
}
