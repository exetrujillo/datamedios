#' Extraccion de noticias de BioBio.cl por rango de fechas
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias de BioBio.cl utilizando un rango de fechas.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param fecha_inicio Fecha de inicio del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_fecha_bbcl(
#'   "inteligencia artificial", "2025-01-01",
#'   "2025-02-24"
#' )
#' }
#' @export

extraer_noticias_fecha_bbcl <- function(search_query, fecha_inicio, fecha_fin) {
  # Inicializamos variables
  # Inicializamos variables
  offset <- 0
  total_results <- 0
  all_data <- crear_df_vacio()

  lista_resultados <- list()

  # Encabezados para la solicitud
  headers <- c(
    `User-Agent` = get_random_user_agent(),
    `Accept` = "application/json, text/plain, */*",
    `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", URLencode(search_query)),
    `Content-Type` = "application/json; charset=UTF-8"
  )

  # Helpers internos para busqueda
  # Usamos el helper centralizado en utils-bbcl-helpers.R: obtener_fecha_offset_bbcl

  # Obtenemos la respuesta inicial
  tryCatch(
    {
      respuesta_inicial <- init_req_bbcl(search_query)
      fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$raw_post_date[1])
      total_results <- as.integer(respuesta_inicial$total)

      if (total_results > 0) {
        message(paste0("Total de resultados posibles: ", total_results))
        f_reciente_date <- as.Date(fecha_mas_reciente)
        f_fin_target <- as.Date(fecha_fin)

        if (f_reciente_date > f_fin_target) {
          wrapper_get_fecha <- function(off) {
            obtener_fecha_offset_bbcl(off, search_query, headers)
          }
          offset <- buscar_offset_limite(f_fin_target, total_results, f_reciente_date, wrapper_get_fecha, batch_size = 20)
        }
      } else {
        warning("No se encontraron noticias con la search query especificada.")
        return(all_data)
      }
    },
    error = function(e) {
      stop("Error inicializando busqueda en BioBio: ", e$message)
    }
  )

  ## Bucle para iterar sobre las paginas de resultados
  repeat {
    # URL de solicitud
    url <- paste0(
      "https://www.biobiochile.cl/lista/api/buscador?offset=", offset,
      "&search=", utils::URLencode(search_query),
      "&intervalo=&orden=ultimas"
    )

    skip_iteration <- FALSE
    tryCatch(
      {
        response <- httr::GET(url, httr::add_headers(.headers = headers))
        if (response$status_code != 200) {
          warning("Error en la solicitud: codigo de estado ", response$status_code)
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

    # Parseo de la respuesta
    data <- tryCatch(
      {
        httr::content(response, "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON(flatten = TRUE)
      },
      error = function(e) {
        warning("Error parseando JSON de BioBio: ", e$message)
        NULL
      }
    )

    if (is.null(data)) {
      offset <- offset + 20
      if (offset >= total_results) break
      next
    }

    # Salimos del bucle si no hay mas datos
    if (is.null(data$notas) || length(data$notas) == 0) break

    # Convertimos las fechas de las noticias
    data$notas$raw_post_date <- as.Date(data$notas$raw_post_date)

    # Filtramos las noticias dentro del rango de fechas
    noticias_filtradas <- data$notas[data$notas$raw_post_date >= as.Date(fecha_inicio) &
      data$notas$raw_post_date <= as.Date(fecha_fin), ]
    if (nrow(noticias_filtradas) > 0) {
      # Asegurar tipo character para ID
      if ("ID" %in% names(noticias_filtradas)) {
        noticias_filtradas$ID <- as.character(noticias_filtradas$ID)
      }
      # Asegurar fecha como character
      noticias_filtradas$raw_post_date <- as.character(noticias_filtradas$raw_post_date)

      # Agregamos noticias filtradas a la lista
      lista_resultados[[length(lista_resultados) + 1]] <- noticias_filtradas
    } else {
      # Logica de parada
      fecha_reciente <- max(data$notas$raw_post_date, na.rm = TRUE)

      if (is.na(fecha_reciente)) {
        warning("Fecha reciente es NA en offset ", offset)
      } else if (fecha_reciente < as.Date(fecha_inicio)) {
        
        break # Salimos del bucle si la fecha mas reciente es anterior a fecha_inicio
      }
    }

    offset <- offset + 20
    if (offset >= total_results) break
  }

  # Unir resultados
  if (length(lista_resultados) > 0) {
    all_data_raw <- dplyr::bind_rows(lista_resultados)
    all_data <- all_data_raw
  } else {
    return(all_data)
  }

  all_data <- procesar_data_bbcl(all_data, search_query)

  ###############################

  invisible(all_data)
}
