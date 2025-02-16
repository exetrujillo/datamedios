#' Generar una tabla estilizada con las palabras mas frecuentes
#'
#' Esta funcion procesa la columna 'contenido_limpio' de un dataframe, tokeniza el texto,
#' cuenta la frecuencia de cada palabra y genera una tabla con las palabras mas frecuentes.
#'
#' @param datos Data frame que contiene la columna 'contenido_limpio'.
#' @param max_words Numero maximo de palabras que se mostraran en la tabla.
#' @param stop_words Vector opcional de palabras que se deben excluir del conteo.
#' @return Una tabla con las palabras mas frecuentes.
#' @examples
#' datos <- data.frame(
#'   contenido_limpio = c(
#'     "La ministra de Defensa Maya Fernandez enfrenta cuestionamientos
#'     el presidente Gabriel Boric solicita transparencia en los procesos.
#'     Renovacion Nacional pide la renuncia de Maya Fernandez debido a la polemica.
#'     La transparencia es fundamental en la politica y la gestion publica"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Probar la funcion con el dataframe de ejemplo
#' tabla_frecuencia_palabras(datos, max_words = 5, stop_words = c())
#' @export

tabla_frecuencia_palabras <- function(datos, max_words, stop_words = NULL) {
  # Validar que 'datos' sea un data frame
  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data frame.")
  }
  # Validar que 'contenido_limpio' exista en los datos
  if (!"contenido_limpio" %in% colnames(datos)) {
    stop("'datos' debe contener una columna llamada 'contenido_limpio'.")
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
    tidytext::unnest_tokens(word, contenido_limpio)

  # Filtrar stop words si se proporcionan
  if (!is.null(stop_words)) {
    stop_words_df <- data.frame(word = stop_words)
    words <- words %>%
      dplyr::anti_join(stop_words_df, by = "word")
  }

  # Calcular frecuencia de palabras
  word_counts <- words %>%
    dplyr::count(word, sort = TRUE)

  # Filtrar las palabras mas frecuentes
  word_counts_filtered <- word_counts %>%
    dplyr::slice_max(n, n = max_words, with_ties = FALSE)

  # Generar la tabla estilizada
  word_counts_filtered %>%
    DT::datatable(
      options = list(
        pageLength = 10,            # Numero de filas por pagina
        autoWidth = TRUE,           # Ajusta el ancho de las columnas automaticamente
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Centra el contenido de las celdas
        )
      ),
      colnames = c("Palabra", "Frecuencia"),  # Nombres de columnas
      class = "display"  # Estilo de tabla
    )
}
