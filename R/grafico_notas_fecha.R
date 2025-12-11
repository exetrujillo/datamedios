#' Grafico de notas por periodo (interactivo)
#'
#' Esta funcion genera un grafico interactivo que muestra la cantidad de publicaciones
#' agrupadas por dia o por mes, con opcion de tema dark o light.
#'
#' @param datos Data frame con los datos procesados, que debe incluir la columna `fecha` en formato YYYY-MM-DD.
#' @param titulo Texto que aparecera en el titulo del grafico.
#' @param fecha_inicio Fecha de inicio para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param fecha_fin Fecha de finalizacion para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param agrupar_por Cadena de texto que especifica el periodo de agrupacion.
#'   Valores validos son `"day"` (por defecto) o `"month"`.
#' @param grosor_linea Ancho de la linea del grafico (por defecto 0.8).
#' @param tamano_punto Tamano de los puntos del grafico (por defecto 1.5).
#' @return Un grafico ggiraph interactivo que muestra la cantidad de publicaciones por el periodo seleccionado.
#' @examples
#' \dontrun{
#' # Ejemplo con tema dark, agrupando por mes
#' datos <- extraer_noticias_fecha("cambio climatico", "2024-01-01", "2025-01-01", subir_a_bd = FALSE)
#' grafico_notas_fecha(datos,
#'   titulo = "Cambio Climatico (por mes)",
#'   agrupar_por = "month", tema = "dark"
#' )
#'
#' # Ejemplo con tema light, agrupando por dia
#' grafico_notas_fecha(datos,
#'   titulo = "Cambio Climatico (por dia)",
#'   fecha_inicio = "2024-01-01", fecha_fin = "2024-03-31",
#'   tema = "light"
#' )
#' }
#'
#' @export
grafico_notas_fecha <- function(datos, titulo, fecha_inicio = NULL, fecha_fin = NULL,
                                agrupar_por = "day", tema = "light", grosor_linea = 0.8, tamano_punto = 1.5) {
  # Cargar librerias necesarias
  if (!requireNamespace("ggiraph", quietly = TRUE)) {
    stop("El paquete 'ggiraph' es necesario para esta funcion. instalalo con: install.packages('ggiraph')")
  }

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
  # Validar el parametro 'agrupar_por'
  if (!agrupar_por %in% c("month", "day")) {
    stop("'agrupar_por' debe ser 'month' o 'day'.")
  }
  # Validar el parametro 'tema'
  if (!tema %in% c("light", "dark")) {
    stop("'tema' debe ser 'light' o 'dark'.")
  }

  # --- 2. Configuracion de Colores por Tema ---

  if (tema == "dark") {
    colores <- list(
      fondo = "#1a1a1a",
      texto = "#ffffff",
      grilla = "#404040",
      linea_principal = "#00d4ff",
      puntos = "#ff6b35",
      suavizado = "#39ff14",
      barras = "#00d4ff"
    )
  } else { # tema == "light"
    colores <- list(
      fondo = "#ffffff",
      texto = "#333333",
      grilla = "#e0e0e0",
      linea_principal = "#1f77b4",
      puntos = "#d62728",
      suavizado = "#2ca02c",
      barras = "#1f77b4"
    )
  }

  # --- 3. Preparacion y Filtrado de Datos ---

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

  # --- 4. Agregacion de Datos Condicional ---

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

  # --- 5. Generacion del Grafico Base con ggplot2 ---

  # Crear el grafico base
  grafico_base <- ggplot2::ggplot(publicaciones_agrupadas, ggplot2::aes(x = fecha_grupo, y = cantidad)) +
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = "Cantidad de Notas"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = colores$fondo, color = NA),
      panel.background = ggplot2::element_rect(fill = colores$fondo, color = NA),
      text = ggplot2::element_text(color = colores$texto),
      axis.text = ggplot2::element_text(color = colores$texto),
      axis.title = ggplot2::element_text(color = colores$texto),
      plot.title = ggplot2::element_text(color = colores$texto, hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = ggplot2::element_line(color = colores$grilla, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = colores$grilla, linewidth = 0.1),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # --- 6. Conversion a ggiraph interactivo ---

  # Añadir las capas (geoms) segun el tipo de agrupacion usando ggiraph
  if (agrupar_por == "month") {
    grafico_ggplot <- grafico_base +
      ggiraph::geom_line_interactive(color = colores$linea_principal, linewidth = grosor_linea) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Fecha: ", format(fecha_grupo, "%Y-%m"), "\nCantidad: ", cantidad)),
        color = colores$puntos, size = tamano_punto
      ) +
      ggplot2::geom_smooth(method = "loess", color = colores$suavizado, se = FALSE, linewidth = 1)
  } else { # agrupar_por == "day"
    grafico_ggplot <- grafico_base +
      ggiraph::geom_col_interactive(
        ggplot2::aes(tooltip = paste0("Fecha: ", format(fecha_grupo, "%Y-%m-%d"), "\nCantidad: ", cantidad)),
        fill = colores$barras, color = colores$barras, width = 1, alpha = 0.8
      )
  }

  # Convertir a ggiraph
  grafico_interactivo <- ggiraph::girafe(
    ggobj = grafico_ggplot,
    width_svg = 9,
    height_svg = 6,
    options = list(
      ggiraph::opts_hover(css = "stroke-width:1px; stroke:black;"),
      ggiraph::opts_hover_inv(css = "opacity:0.3;"),
      ggiraph::opts_tooltip(
        css = paste0("background-color:", colores$fondo, "; color:", colores$texto, "; padding:5px; border-radius:3px; border: 1px solid ", colores$texto),
        opacity = 0.9
      )
    )
  )

  # --- 7. Devolver el Grafico interactivo ---
  return(grafico_interactivo)
}
