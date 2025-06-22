#' Grafico de comparacion de medios por periodo (Interactivo)
#'
#' Esta funcion genera un grafico interactivo que compara la cantidad de publicaciones
#' entre diferentes medios de medios, agrupadas por dia o por mes, con opcion de tema dark o light.
#'
#' @param datos Data frame con los datos procesados, que debe incluir las columnas `fecha` y `medio`.
#' @param titulo Texto que aparecera en el titulo del grafico.
#' @param fecha_inicio Fecha de inicio para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param fecha_fin Fecha de finalizacion para la construccion del grafico en formato YYYY-MM-DD (opcional).
#' @param medios Vector de strings con las medios a comparar. Si es NULL, usa todas las medios disponibles.
#' @param agrupar_por Cadena de texto que especifica el periodo de agrupacion.
#'   Valores validos son `"day"` (por defecto) o `"month"`.
#' @param tema Tema del grafico. Valores validos son `"light"` (por defecto) o `"dark"`.
#' @param tipo_grafico Tipo de visualizacion. Valores validos son `"lineas"` (por defecto) o `"barras"`.
#' @return Un grafico plotly interactivo que muestra la comparacion de publicaciones por medio y periodo.
#' @examples
#' \dontrun{
#' # Comparar todas las medios por mes
#' datos <- extraer_noticias_fecha("delincuencia", "2024-01-01", "2025-01-01", subir_a_bd = FALSE)
#' grafico_comparacion_medios(datos, titulo = "Cobertura de Delincuencia por Medio",
#'                             agrupar_por = "month", tema = "dark")
#'
#' # Comparar medios especificas por dia
#' grafico_comparacion_medios(datos, titulo = "Comparacion BBCl vs emol",
#'                             medios = c("bbcl", "emol"),
#'                             fecha_inicio = "2024-06-01", fecha_fin = "2024-06-30",
#'                             agrupar_por = "day", tipo_grafico = "barras")
#' }
#'
#' @export
grafico_comparacion_medios <- function(datos, titulo, fecha_inicio = NULL, fecha_fin = NULL,
                                        medios = NULL, agrupar_por = "day", tema = "light",
                                        tipo_grafico = "lineas") {

  # Cargar librerias necesarias
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("el paquete 'plotly' es necesario para esta funcion. Instalalo con: install.packages('plotly')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("el paquete 'dplyr' es necesario para esta funcion. Instalalo con: install.packages('dplyr')")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("el paquete 'lubridate' es necesario para esta funcion. Instalalo con: install.packages('lubridate')")
  }

  # --- 1. Validacion de entradas ---

  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'fecha' y 'medio' existan en los datos
  if (!"fecha" %in% colnames(datos)) {
    stop("'datos' debe contener la columna 'fecha'.")
  }
  if (!"medio" %in% colnames(datos)) {
    stop("'datos' debe contener la columna 'medio'.")
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

  # Validar el parametro 'tipo_grafico'
  if (!tipo_grafico %in% c("lineas", "barras")) {
    stop("'tipo_grafico' debe ser 'lineas' o 'barras'.")
  }

  # --- 2. Preparacion y Filtrado de Datos ---

  # Convertir 'fecha' a formato fecha
  datos_proc <- datos %>%
    dplyr::mutate(fecha = as.Date(fecha, format = "%Y-%m-%d"))

  # Filtrar por medios si se especifican
  if (!is.null(medios)) {
    medios_disponibles <- unique(datos_proc$medio)
    medios_no_encontradas <- setdiff(medios, medios_disponibles)

    if (length(medios_no_encontradas) > 0) {
      warning(paste("Las siguientes medios no se encontraron en los datos:",
                    paste(medios_no_encontradas, collapse = ", ")))
    }

    medios_validas <- intersect(medios, medios_disponibles)
    if (length(medios_validas) == 0) {
      stop("Ninguna de las medios especificadas se encontro en los datos.")
    }

    datos_proc <- datos_proc %>%
      dplyr::filter(medio %in% medios_validas)
  }

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
    datos_filtrados <- datos_proc
  }

  # Verificar si hay datos despues del filtrado
  if (nrow(datos_filtrados) == 0) {
    stop("No hay datos en el rango de fechas y medios seleccionadas.")
  }

  # --- 3. Configuracion de Colores y Paletas ---

  medios_unicas <- sort(unique(datos_filtrados$medio))
  n_medios <- length(medios_unicas)

  if (tema == "dark") {
    colores_tema <- list(
      fondo = "#1a1a1a",
      texto = "#ffffff",
      grilla = "#404040"
    )
    # Paleta de colores vibrantes para tema dark
    paleta_medios <- c("#00d4ff", "#ff6b35", "#39ff14", "#ff1493", "#ffd700",
                        "#9370db", "#00fa9a", "#ff4500", "#1e90ff", "#ff69b4")
  } else {
    colores_tema <- list(
      fondo = "#ffffff",
      texto = "#333333",
      grilla = "#e0e0e0"
    )
    # Paleta de colores contrastantes para tema light
    paleta_medios <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd",
                        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  }

  # Asignar colores a cada medio
  colores_medios <- setNames(paleta_medios[1:n_medios], medios_unicas)

  # --- 4. Agregacion de Datos por medio ---

  if (agrupar_por == "month") {
    # Agrupar datos por mes y medio
    publicaciones_agrupadas <- datos_filtrados %>%
      dplyr::mutate(fecha_grupo = lubridate::floor_date(fecha, "month")) %>%
      dplyr::group_by(fecha_grupo, medio) %>%
      dplyr::summarise(cantidad = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(fecha_grupo, medio)
  } else {
    # Agrupar datos por dia y medio
    publicaciones_agrupadas <- datos_filtrados %>%
      dplyr::group_by(fecha, medio) %>%
      dplyr::rename(fecha_grupo = fecha) %>%
      dplyr::summarise(cantidad = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(fecha_grupo, medio)
  }

  # Completar fechas faltantes con 0 para cada medio
  fechas_completas <- seq(min(publicaciones_agrupadas$fecha_grupo),
                          max(publicaciones_agrupadas$fecha_grupo),
                          by = if(agrupar_por == "month") "month" else "day")

  grid_completo <- expand.grid(fecha_grupo = fechas_completas,
                               medio = medios_unicas,
                               stringsAsFactors = FALSE)

  publicaciones_agrupadas <- grid_completo %>%
    dplyr::left_join(publicaciones_agrupadas, by = c("fecha_grupo", "medio")) %>%
    dplyr::mutate(cantidad = ifelse(is.na(cantidad), 0, cantidad)) %>%
    dplyr::arrange(fecha_grupo, medio)

  # --- 5. Creacion de Tooltips Personalizados ---

  if (agrupar_por == "month") {
    publicaciones_agrupadas <- publicaciones_agrupadas %>%
      dplyr::mutate(
        fecha_texto = format(fecha_grupo, "%Y-%m"),
        hover_text = paste0("medio: ", medio, "<br>",
                            "Fecha: ", fecha_texto, "<br>",
                            "Cantidad: ", cantidad)
      )
  } else {
    publicaciones_agrupadas <- publicaciones_agrupadas %>%
      dplyr::mutate(
        fecha_texto = format(fecha_grupo, "%Y-%m-%d"),
        hover_text = paste0("medio: ", medio, "<br>",
                            "Fecha: ", fecha_texto, "<br>",
                            "Cantidad: ", cantidad)
      )
  }

  # --- 6. Creacion del Grafico con Plotly Nativo ---

  # Crear grafico base
  grafico_plotly <- plotly::plot_ly()

  if (tipo_grafico == "lineas") {
    # Agregar lineas para cada medio
    for (medio_actual in medios_unicas) {
      datos_medio <- publicaciones_agrupadas %>%
        dplyr::filter(medio == medio_actual)

      grafico_plotly <- grafico_plotly %>%
        plotly::add_trace(
          data = datos_medio,
          x = ~fecha_grupo,
          y = ~cantidad,
          type = "scatter",
          mode = "lines+markers",
          name = medio_actual,
          line = list(color = colores_medios[medio_actual], width = 2.5),
          marker = list(color = colores_medios[medio_actual], size = 6),
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  } else { # tipo_grafico == "barras"
    # Crear grafico de barras agrupadas
    for (medio_actual in medios_unicas) {
      datos_medio <- publicaciones_agrupadas %>%
        dplyr::filter(medio == medio_actual)

      grafico_plotly <- grafico_plotly %>%
        plotly::add_trace(
          data = datos_medio,
          x = ~fecha_grupo,
          y = ~cantidad,
          type = "bar",
          name = medio_actual,
          marker = list(color = colores_medios[medio_actual], opacity = 0.8),
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  }

  # --- 7. Configuracion del Layout ---

  grafico_plotly <- grafico_plotly %>%
    plotly::layout(
      title = list(
        text = titulo,
        font = list(color = colores_tema$texto, size = 16, family = "Arial"),
        x = 0.5
      ),
      xaxis = list(
        title = list(text = "Fecha", font = list(color = colores_tema$texto)),
        tickfont = list(color = colores_tema$texto),
        gridcolor = colores_tema$grilla,
        zerolinecolor = colores_tema$grilla
      ),
      yaxis = list(
        title = list(text = "Cantidad de Noticias", font = list(color = colores_tema$texto)),
        tickfont = list(color = colores_tema$texto),
        gridcolor = colores_tema$grilla,
        zerolinecolor = colores_tema$grilla
      ),
      plot_bgcolor = colores_tema$fondo,
      paper_bgcolor = colores_tema$fondo,
      font = list(color = colores_tema$texto, family = "Arial", size = 12),
      hovermode = "x unified",
      legend = list(
        font = list(color = colores_tema$texto),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = colores_tema$texto,
        borderwidth = 1
      ),
      barmode = if(tipo_grafico == "barras") "group" else NULL
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = "png",
        filename = paste0("comparacion_medios_", gsub(" ", "_", tolower(titulo))),
        height = 500,
        width = 900,
        scale = 2
      )
    )

  # --- 8. Devolver el Grafico Interactivo ---
  return(grafico_plotly)
}
