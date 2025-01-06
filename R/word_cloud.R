#' Funcion de visualizaciones
#'
#' Esta funcion incluye varias formas de visualizar los datos obtenidos.
#'
#' @param datos data frame que genera la funcion de extraccion, con el texto ya limpio.
#' @param max_words Cantidad maxima de palabras que apareceran en la nube.
#' @param stop_words Definir las palabras que seran ignoradas en la visualizacion. Puede ser 'es' (realizado por los desarrolladores del paquete), NULL, o una lista.
#' @return Una nube de palabras con las palabras mas frecuentes.
#' @examples
#' \dontrun{
#' datos <- extraer_noticias_fecha("Monsalve", "2024-01-01","2025-01-01", subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos)
#' datos_proc <- limpieza_notas(datos)
#' word_cloud(datos_proc, max_words = 50, stop_words = c("es", "la"))
#' }
#' @export

word_cloud <- function(datos, max_words, stop_words = NULL) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'post_content' exista en los datos
  if (!"post_content" %in% colnames(datos)) {
    stop("'datos' debe contener una columna llamada 'post_content'.")
  }

  # Validar que 'max_words' sea numerico y este definido
  if (missing(max_words) || !is.numeric(max_words)) {
    stop("'max_words' debe ser un numero.")
  }

  # Validar el argumento 'stop_words'
  if (!is.null(stop_words) && !is.character(stop_words)) {
    stop("'stop_words' debe ser un vector de palabras.")
  }

  # Generar los tokens (separar palabras)
  words <- datos %>%
    tidytext::unnest_tokens(word, post_content)

  # Filtrar stop words si se proporcionan
  if (!is.null(stop_words)) {
    stop_words_df <- data.frame(word = stop_words)
    words <- words %>%
      dplyr::anti_join(stop_words_df, by = "word")
  }

  # Verificar si hay palabras suficientes para generar la nube
  if (nrow(words) == 0) {
    stop("No se encontraron palabras para generar la nube.")
  }

  # Calcular frecuencia de palabras
  word_counts <- words %>%
    dplyr::count(word, sort = TRUE)

  # Filtrar las palabras mas frecuentes
  word_counts_filtered <- word_counts %>%
    dplyr::slice_max(n, n = max_words)

  # Generar la nube de palabras
  wordcloud2::wordcloud2(
    data = word_counts_filtered,
    size = 0.3,              # Aumenta el tamano general de las palabras
    minSize = 0,             # Asegura que todas las palabras sean visibles
    gridSize = 1,            # Ajusta la densidad de palabras
    color = "random-dark",   # Colores para las palabras
    backgroundColor = "white", # Fondo blanco
    shape = "circle",        # Forma circular para compactar la nube
    ellipticity = 1          # Elimina la elipse y fuerza un formato mas centrado
  )
}
