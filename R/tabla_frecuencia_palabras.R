#' Generar una tabla estilizada con las palabras más frecuentes
#'
#' Esta función procesa la columna 'post_content' de un dataframe, tokeniza el texto,
#' cuenta la frecuencia de cada palabra y genera una tabla con las palabras más frecuentes.
#'
#' @param datos Data frame que contiene la columna 'post_content'.
#' @param max_words Número máximo de palabras que se mostrarán en la tabla.
#' @param stop_words Vector opcional de palabras que se deben excluir del conteo.
#' @return Una tabla con las palabras más frecuentes.
#' @examples
#' datos <- extraer_noticias_fecha("cambio climatico", "2024-01-01","2025-01-01", subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos)
#' datos <- limpieza_notas(datos)
#' tabla_frecuencia_palabras(datos, max_words = 5, stop_words = c("el", "de"))
#' @export

tabla_frecuencia_palabras <- function(datos, max_words, stop_words = NULL) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }

  # Validar que 'post_content' exista en los datos
  if (!"post_content" %in% colnames(datos)) {
    stop("'datos' debe contener una columna llamada 'post_content'.")
  }

  # Validar que 'max_words' sea numérico y esté definido
  if (missing(max_words) || !is.numeric(max_words)) {
    stop("'max_words' debe ser un número.")
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

  # Calcular frecuencia de palabras
  word_counts <- words %>%
    dplyr::count(word, sort = TRUE)

  # Filtrar las palabras más frecuentes
  word_counts_filtered <- word_counts %>%
    dplyr::slice_max(n, n = max_words)

  # Generar la tabla estilizada
  word_counts_filtered %>%
    DT::datatable(
      options = list(
        pageLength = 10,            # Número de filas por página
        autoWidth = TRUE,           # Ajusta el ancho de las columnas automáticamente
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Centra el contenido de las celdas
        )
      ),
      colnames = c("Palabra", "Frecuencia"),  # Nombres de columnas
      class = "display"  # Estilo de tabla
    )
}
