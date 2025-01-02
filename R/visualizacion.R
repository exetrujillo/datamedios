#' Función de visualizaciones
#'
#' Esta función incluye varias formas de visualizar los datos obtenidos.
#'
#' @param datos data frame donde se desea aplicar los gráficos.
#' @param tipo Tipo de visualización que se desea (word_cloud, time_frecuency, top_words, tf-idf, heatmap).
#' @param stop_words Definir las palabras que serán ignoradas en la visualización. Puede ser es (realizado por los desarrolladores del paquete), null, o una lista.
#' @param ... Parámetros adicionales necesarios para ciertos tipos de visualización (e.g., para word_cloud).
#' @return Un gráfico con características dependientes del tipo especificado y los datos ingresados.
#' @examples
#' datos <- extraer_noticias_fecha("cambio climatico", "2024-01-01","2025-01-01")
#' datos <- extraccion_parrafos(datos)
#' datos_proc <- limpieza_notas(datos)
#' visualizacion(datos_proc, word_cloud, stop_words=es)
#' @export

visualizacion <- function(datos, tipo, stop_words, ...) {
  # Validar que 'tipo' es uno de los permitidos
  tipo <- match.arg(tipo, c("word_cloud", "time_frecuency", "top_words", "tf-idf", "heatmap"))

  # Validar que 'datos' es un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Parámetros adicionales
  args <- list(...)

  # Lógica para cada tipo de visualización
  if (tipo == "word_cloud") {
    max_words <- args$max_words
    stop_words <- args$stop_words
    if (is.null(args$max_words)) {
      stop("Para 'word_cloud', debes especificar 'max_words' como un parámetro adicional.")
    }
    if (!is.numeric(args$max_words)) {
      stop("'max_words' debe ser un número.")
    }
    # Validar el argumento 'stop_words'
    if (!is.null(stop_words) && !is.character(stop_words) && !is.list(stop_words)) {
      stop("'stop_words' debe ser 'es', NULL, o una lista de palabras.")
    }
    message("Generando word cloud...")
    return(list(tipo = "word_cloud", params = args))
  }

  if (tipo == "time_frecuency") {
    # Generar gráfico de frecuencia temporal
    message("Generando gráfico de frecuencia temporal...")
    return(list(tipo = "time_frecuency"))
  }

  if (tipo == "top_words") {
    # Generar gráfico de palabras más frecuentes
    message("Generando gráfico de palabras más frecuentes...")
    return(list(tipo = "top_words"))
  }

  if (tipo == "tf-idf") {
    # Generar análisis de TF-IDF
    message("Generando análisis de TF-IDF...")
    return(list(tipo = "tf-idf"))
  }

  if (tipo == "heatmap") {
    # Generar mapa de calor
    message("Generando heatmap...")
    return(list(tipo = "heatmap"))
  }

  stop("Tipo de visualización desconocido.")
}
