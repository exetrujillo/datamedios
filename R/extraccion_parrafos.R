#' Extraer parrafos de una columna de texto
#'
#' Esta funcion procesa una columna de texto en un dataframe y extrae los parrafos que coinciden con los sinonimos proporcionados.
#'
#' @param datos Data frame que contiene los datos de entrada con la columna "contenido".
#' @param sinonimos Vector de sinonimos que se incluiran en la busqueda.
#' @return Data frame con una columna adicional 'parrafos_filtrados' que contiene los parrafos extraidos como listas.
#' @examples
#' \dontrun{
#' datos <- extraer_noticias_max_res("inteligencia artificial", max_resultas = 140, subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos, sinonimos = c("IA", "AI"))
#' }
#' @export

extraccion_parrafos <- function(datos, sinonimos = c()) {
  # Verificar que los datos sean un data frame y que contengan la columna 'post_content'
  if (!is.data.frame(datos)) stop("'datos' debe ser un data frame.")
  if (!"contenido" %in% colnames(datos)) stop("El data frame debe contener la columna 'contenido'.")

  if (!is.null(sinonimos)){
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], "|", paste(sinonimos, collapse = "|"), ")\\b")
  } else {
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], ")\\b")
  }

  # Procesar cada contenido de 'contenido' para extraer parrafos filtrados
  datos <- datos %>%
    dplyr::mutate(
      parrafos_filtrados = purrr::map(contenido, ~ {
        # Manejo de errores para lectura HTML
        nodo_html <- tryCatch(rvest::read_html(.x), error = function(e) return(NA))

        if (!is.na(nodo_html)) {
          # Extraer parrafos del HTML
          parrafos <- nodo_html %>% rvest::html_elements("p") %>% rvest::html_text2()

          # Filtrar parrafos que coincidan con los sinonimos
          parrafos[grepl(pattern, parrafos)]
        } else {
          return(NA) # En caso de error
        }
      })
    )

  return(datos)
}
