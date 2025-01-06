#' Funcion para limpiar notas de contenido HTML
#'
#' Esta funcion permite limpiar por completo las notas eliminando codigos y secciones irrelevantes.
#' Verifica que el input sea un data frame con una columna llamada `post_content`.
#' @param datos Data frame donde estan almacenadas las notas y con la funcion extraccion_parrafos ya operada.
#' @param sinonimos Una lista de character
#' @return Un dataframe con el post_content limpio
#' @examples
#'
#' \dontrun{
#' datos <- extraer_noticias_max_res("inteligencia artificial", max_results= 150, subir_a_bd = FALSE)
#' datos <- extraccion_parrafos(datos)
#' datos_proc <- limpieza_notas(datos, sinonimos = c("IA", "AI"))
#' }
#' @export

limpieza_notas <- function(datos, sinonimos = c()) {
  # Validacion inicial: verificar que el input sea un data frame con la columna requerida
  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }
  if (!"post_content" %in% colnames(datos)) {
    stop("El data frame debe contener una columna llamada 'post_content'.")
  }
  # Validar el argumento 'stop_words'
  if (!is.null(sinonimos) && !is.character(sinonimos)) {
    stop("'sinonimos' debe ser un vector de palabras.")
  }

  # Iteramos sobre cada fila del data frame
  for (i in seq_len(nrow(datos))) {
    # Convertimos el contenido a un objeto HTML para usar rvest
    contenido_html <- rvest::read_html(datos$post_content[[i]])

    # Eliminamos los divs con contenido irrelevante
    contenido_html %>%
      rvest::html_nodes("div.lee-tambien-bbcl") %>%
      xml2::xml_remove()

    contenido_html %>%
      rvest::html_nodes("blockquote.instagram-media") %>%
      xml2::xml_remove()

    contenido_html %>%
      rvest::html_nodes("blockquote.twitter-tweet") %>%
      xml2::xml_remove()

    # Convertimos el HTML limpio a texto y eliminamos frases residuales
    contenido_texto <- as.character(contenido_html)
    contenido_texto <- stringr::str_replace_all(
      contenido_texto,
      stringr::regex("Lee tambi\u00e9n.*?<\\/div>", dotall = TRUE),
      ""
    )

    # Guardamos el contenido limpio de vuelta en el data frame
    datos$post_content[[i]] <- contenido_texto
  }
  print(nrow(datos))

  if (!is.null(sinonimos)){
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], "|", paste(sinonimos, collapse = "|"), ")\\b")
  } else {
    pattern <- paste0("(?i)\\b(", datos$search_query[[1]], ")\\b")
  }
  # Identificamos las filas que NO contienen el termino de busqueda
  indices_no_match <- which(!stringr::str_detect(datos$post_content, pattern))

  # Eliminamos las filas que no contienen el termino de busqueda
  datos <- datos[-indices_no_match, ]

  print(nrow(datos))

  # Calculamos el numero total de resultados
  total_results <- nrow(datos)
  cat("El numero total de resultados obtenidos es:", total_results, "\n")

  # Procesamos cada nota para extraer y limpiar el texto plano
  for (contador in seq_len(total_results)) {
    datos$post_content_clean[[contador]] <- rvest::read_html(datos$post_content[[contador]]) %>%
      rvest::html_text2() %>%
      stringr::str_squish()
  }

  print(nrow(datos))

  # Mostramos un ejemplo de contenido limpio
  # print(datos$post_content[1])

  return(datos)
}
