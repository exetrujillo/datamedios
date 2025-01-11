#' Extraccion de noticias desde la API de BioBio.cl
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias desde la API de BioBio.cl.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param max_results Numero maximo de resultados a extraer (opcional, por defecto todos).
#' @param subir_a_bd por defecto TRUE, FALSE para test y cosas por el estilo (opcional).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' noticias <- extraer_noticias_max_res("inteligencia artificial",
#' max_results = 100, subir_a_bd = FALSE)
#' @export
extraer_noticias_max_res <- function(search_query, max_results = NULL, subir_a_bd = TRUE) {
  # Validamos los parametros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda valida como texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un numero entero positivo o NULL.")
  }

  # Inicializamos variables
  encoded_query <- URLencode(search_query)
  all_data <- data.frame(
    ID = character(),
    post_title = character(),
    post_content = character(),
    post_excerpt = character(),
    post_URL = character(),
    post_categories = character(),
    post_tags = character(),
    year = integer(),
    month = integer(),
    day = integer(),
    post_category_primary.name = character(),
    post_category_secondary.name = character(),
    post_image.URL = character(),
    post_image.alt = character(),
    post_image.caption = character(),
    author.display_name = character(),
    raw_post_date = as.Date(character()),
    resumen_de_ia = character(),
    search_query = character(),
    stringsAsFactors = FALSE
  )

  # Obtenemos la respuesta inicial
  respuesta_inicial <- init_req_bbcl(search_query)
  fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$raw_post_date[1])
  total_results <- as.integer(respuesta_inicial$total)
  if(total_results > 0){
    print(paste0("Total de resultados posibles: ", total_results))
    print(paste0("Noticia mas reciente disponible es de la fecha: ", fecha_mas_reciente))
  }else{
    stop("No se encontraron noticias con la search query especificada.")
  }

  # Determinamos el numero de resultados a extraer
  if (is.null(max_results) || max_results > total_results) {
    max_results <- total_results
  }

  print(paste0("Se extraeran un maximo de ", max_results, " resultados."))

  # Iteramos para obtener todas las noticias necesarias
  offset <- 0
  while (nrow(all_data) < max_results) {
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", encoded_query,
      "&intervalo=&orden=ultimas"
    )

    response <- httr::GET(url, httr::add_headers(.headers = c(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
      `Accept` = "application/json, text/plain, */*",
      `Content-Type` = "application/json; charset=UTF-8"
    )))

    if (response$status_code != 200) {
      warning("Error al realizar la solicitud. Codigo de estado: ", response$status_code)
      break
    }

    data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (is.null(data$notas) || length(data$notas) == 0) {
      warning("No se encontraron mas notas para extraer.")
      break
    }

    noticias <- as.data.frame(data$notas)
    # Verificar las columnas de noticias
    missing_columns <- setdiff(colnames(all_data), colnames(noticias))

    # Si faltan columnas, anadirlas con valores NA
    for (col in missing_columns) {
      noticias[[col]] <- NA
    }

    # Asegurarse de que las columnas esten en el orden correcto
    noticias <- noticias[, colnames(all_data), drop = FALSE]
    # Anadir las noticias a all_data
    all_data <- rbind(all_data, noticias)

    # Controlar el numero de resultados
    if (nrow(all_data) >= max_results) {
      all_data <- all_data[1:max_results, ]
      break
    }

    offset <- offset + 20
  }

  all_data$search_query <- tolower(search_query)
  all_data$raw_post_date <- as.Date(all_data$raw_post_date)

  # Subimos a la base de datos en caso de que el parametro subir_a_db es TRUE
  if (subir_a_bd) {
    tryCatch({
      # Llamamos a la funcion que sube los datos si subir_a_bd es TRUE
      agregar_datos_unicos(all_data)
    }, error = function(e) {
      message("Ocurrio un error al intentar agregar los datos a la base de datos: ", e$message)
    })
  }

  print(paste0("Noticias hasta la fecha: ", all_data$raw_post_date[nrow(all_data)]))

  return(all_data)
}
