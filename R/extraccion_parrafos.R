#' Extraer parrafos de una columna de texto
#'
#' Esta funcion procesa una columna de texto en un dataframe y extrae los parrafos que coinciden con los sinonimos proporcionados.
#'
#' @param datos Data frame que contiene los datos de entrada con la columna "contenido".
#' @param sinonimos Vector de sinonimos que se incluiran en la busqueda.
#' @return Data frame con una columna adicional 'parrafos_filtrados' que contiene los parrafos extraidos como listas.
#' @examples
#' \donttest{
#' datos <- extraer_noticias_max_res("inteligencia artificial", max_results = 140, subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos, sinonimos = c("IA", "AI"))
#' }
#' @export

extraccion_parrafos <- function(datos, sinonimos = c()) {
  if (!is.data.frame(datos)) stop("'datos' debe ser un data frame.")
  if (!"contenido" %in% colnames(datos)) stop("El data frame debe contener la columna 'contenido'.")

  if (!is.null(sinonimos)){
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], "|", paste(sinonimos, collapse = "|"), ")\\b")
  } else {
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], ")\\b")
  }

  datos <- datos %>%
    dplyr::mutate(
      parrafos_filtrados = purrr::map(contenido, ~ {
        # Validar si el contenido es NA o vacio
        if (is.na(.x) || !nzchar(.x)) return(NA)

        # Detectar si es HTML o Texto Plano
        is_html <- grepl("<[^>]+>", .x)
        
        texto_a_filtrar <- character()
        
        if (is_html) {
           nodo_html <- tryCatch(rvest::read_html(.x), error = function(e) return(NA))
           if (!is.na(nodo_html) && length(nodo_html) > 0) {
              texto_a_filtrar <- nodo_html %>% rvest::html_elements("p") %>% rvest::html_text2()
           }
        } else {
           # Es texto plano, quizas separado por saltos de linea
           texto_a_filtrar <- unlist(strsplit(.x, "\n"))
        }

        if (length(texto_a_filtrar) > 0) {
           texto_a_filtrar[grepl(pattern, texto_a_filtrar)]
        } else {
           return(NA)
        }
      })
    )

  return(datos)
}
