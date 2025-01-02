#' Extraer párrafos de una columna de texto
#'
#' Esta función procesa una columna de texto en un dataframe y extrae los párrafos que coinciden con los sinónimos proporcionados.
#'
#' @param datos Data frame que contiene los datos de entrada con la columna "post_content".
#' @param sinonimos Vector de sinónimos que se incluirán en la búsqueda.
#' @return Data frame con una columna adicional 'parrafos_filtrados' que contiene los párrafos extraídos como listas.
#' @examples
#' \dontrun{
#' datos <- extraer_noticias_max_res("inteligencia artificial", max_resultas = 140)
#' datos <- extraccion_parrafos(datos, sinonimos = c("IA", "AI"))
#' }
#' @export

extraccion_parrafos <- function(datos, sinonimos = c()) {
  # Verificar que los datos sean un data frame y que contengan la columna 'post_content'
  if (!is.data.frame(datos)) stop("'datos' debe ser un data frame.")
  if (!"post_content" %in% colnames(datos)) stop("El data frame debe contener la columna 'post_content'.")

  if (!is.null(sinonimos)){
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], "|", paste(sinonimos, collapse = "|"), ")\\b")
  } else {
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], ")\\b")
  }

  # Procesar cada contenido de 'post_content' para extraer párrafos filtrados
  datos <- datos %>%
    dplyr::mutate(
      parrafos_filtrados = purrr::map(post_content, ~ {
        # Manejo de errores para lectura HTML
        nodo_html <- tryCatch(rvest::read_html(.x), error = function(e) return(NA))

        if (!is.na(nodo_html)) {
          # Extraer párrafos del HTML
          parrafos <- nodo_html %>% rvest::html_elements("p") %>% rvest::html_text2()

          # Filtrar párrafos que coincidan con los sinónimos
          parrafos[grepl(pattern, parrafos)]
        } else {
          return(NA) # En caso de error
        }
      })
    )

  return(datos)
}
