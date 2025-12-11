#' Extraccion de noticias de BioBio.cl por cantidad maxima de resultados
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias de BioBio.cl
#' entregando como parametro una cantidad maxima de resultados.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param max_results Cantidad maxima de resultados (opcional).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_fecha_bbcl(
#'   "inteligencia artificial", "2025-01-01",
#'   "2025-02-24"
#' )
#' }
#' @export
extraer_noticias_max_res_bbcl <- function(search_query, max_results = NULL) {
  # Inicializamos variables
  encoded_query <- URLencode(search_query)

  # Estructura vacia
  all_data <- crear_df_vacio()
  lista_resultados <- list()
  count_results <- 0

  # Encabezados para la solicitud
  headers <- c(
    `User-Agent` = get_random_user_agent(),
    `Accept` = "application/json, text/plain, */*",
    `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", URLencode(search_query)),
    `Content-Type` = "application/json; charset=UTF-8"
  )

  # Obtenemos la respuesta inicial
  tryCatch(
    {
      respuesta_inicial <- init_req_bbcl(search_query)
      fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$raw_post_date[1])
      total_results <- as.integer(respuesta_inicial$total)
      if (total_results > 0) {
        message(paste0("Total de resultados disponibles en bbcl: ", total_results))
        message(paste0("Noticia mas reciente disponible en bbcl es de la fecha: ", fecha_mas_reciente))
      } else {
        warning("No se encontraron noticias con la search query especificada.")
        return(all_data)
      }
    },
    error = function(e) {
      stop("Error inicializando busqueda en BioBio: ", e$message)
    }
  )

  # Determinamos el numero de resultados a extraer
  if (is.null(max_results) || max_results > total_results) {
    max_results <- total_results
  }

  # Iteramos para obtener todas las noticias necesarias
  offset <- 0

  while (count_results < max_results) {
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", encoded_query,
      "&intervalo=&orden=ultimas"
    )

    skip_iteration <- FALSE
    tryCatch(
      {
        response <- httr::GET(url, httr::add_headers(.headers = c(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
          `Accept` = "application/json, text/plain, */*",
          `Content-Type` = "application/json; charset=UTF-8"
        )))

        if (response$status_code != 200) {
          warning("Error al realizar la solicitud. Codigo de estado: ", response$status_code)
          skip_iteration <- TRUE
        }
      },
      error = function(e) {
        warning("Error de conexion en BioBio: ", e$message)
        skip_iteration <- TRUE
      }
    )

    if (skip_iteration) {
      offset <- offset + 20
      if (offset >= total_results) break
      next
    }

    data <- tryCatch(
      {
        httr::content(response, "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(flatten = TRUE)
      },
      error = function(e) {
        warning("Error parseando JSON: ", e$message)
        NULL
      }
    )

    if (is.null(data)) {
      offset <- offset + 20
      if (offset >= total_results) break
      next
    }

    if (is.null(data$notas) || length(data$notas) == 0) {
      if (count_results == 0) warning("No se encontraron mas notas para extraer.")
      break
    }

    noticias <- as.data.frame(data$notas)

    # Asegurar tipos
    if ("ID" %in% names(noticias)) {
      noticias$ID <- as.character(noticias$ID)
    }

    # Anadir las noticias a la lista
    lista_resultados[[length(lista_resultados) + 1]] <- noticias
    count_results <- count_results + nrow(noticias)

    offset <- offset + 20
  }

  # Unir resultados
  if (length(lista_resultados) > 0) {
    all_data <- dplyr::bind_rows(lista_resultados)

    # Controlar max_results exacto
    if (nrow(all_data) > max_results) {
      all_data <- all_data[1:max_results, ]
    }
  }

  all_data <- procesar_data_bbcl(all_data, search_query)

  ###############################

  return(all_data)
}
