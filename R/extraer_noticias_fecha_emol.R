#' Extraccion de noticias de emol.com por rango de fechas
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias de emol.com utilizando un rango de fechas.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param fecha_inicio Fecha de inicio del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fuente Fuente de emol para iterar (obligatoria).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_fecha_emol("inteligencia artificial", "2025-01-01",
#'   "2025-02-24",
#'   fuente = "emol"
#' )
#' }
#' @export

extraer_noticias_fecha_emol <- function(search_query, fecha_inicio, fecha_fin, fuente) {
  #### Inicializamos variables ####
  
  # Logica recursiva para 'emol-todas'
  if (fuente == "emol-todas") {
    fuentes_loop <- get_fuentes_emol()
    lista_res <- list()
    
    for (f in fuentes_loop) {
      tryCatch({
        message(paste("Iniciando extraccion recursiva para fuente:", f))
        df_temp <- extraer_noticias_fecha_emol(search_query, fecha_inicio, fecha_fin, fuente = f)
        if (!is.null(df_temp) && nrow(df_temp) > 0) {
          lista_res[[length(lista_res) + 1]] <- df_temp
        }
      }, error = function(e) {
        message(paste("Error en extraccion recursiva para fuente", f, ":", e$message))
      })
    }
    
    if (length(lista_res) > 0) {
      return(dplyr::bind_rows(lista_res))
    } else {
      return(crear_df_vacio())
    }
  }

  offset <- 0
  total_results <- 0
  fecha_mas_reciente <- NA
  lista_resultados <- list()

  # Estructura vacia por defecto
  all_data <- crear_df_vacio()

  # Obtener la respuesta inicial
  respuesta_inicial <- tryCatch(
    {
      init_req_emol(search_query, fuentes = fuente)
    },
    error = function(e) {
      message("Error al obtener la respuesta inicial: ", e$message)
      return(NULL)
    }
  )

  # Verificar si hay resultados en la respuesta inicial
  if (is.null(respuesta_inicial) || nrow(respuesta_inicial) == 0) {
    message(paste("No se encontraron resultados iniciales para la fuente:", fuente))
    return(crear_df_vacio())
  }

  # Calcular total de resultados
  total_results <- tryCatch(
    {
      as.numeric(respuesta_inicial$total[1])
    },
    error = function(e) {
      message("No se pudo convertir total de resultados: ", e$message)
      NA_real_
    }
  )

  # Obtener fecha mas reciente
  fecha_mas_reciente_raw <- NA
  if ("_source.fechaPublicacion" %in% names(respuesta_inicial)) {
    fecha_mas_reciente_raw <- respuesta_inicial$`_source.fechaPublicacion`[1]
  } else if ("_source.fechaModificacion" %in% names(respuesta_inicial)) {
    fecha_mas_reciente_raw <- respuesta_inicial$`_source.fechaModificacion`[1]
  }

  if (!is.na(fecha_mas_reciente_raw)) {
    fecha_mas_reciente <- lubridate::ymd_hms(fecha_mas_reciente_raw)
  }

  if (!is.na(total_results) && total_results > 0 && !is.na(fecha_mas_reciente)) {
    f_reciente_date <- lubridate::as_date(fecha_mas_reciente)
    f_fin_target <- lubridate::as_date(fecha_fin)

    if (f_reciente_date > f_fin_target) {
      total_pages <- ceiling(total_results / 10)

      funcion_get_fecha_emol <- function(page_idx) {
        Sys.sleep(0.2)
        tryCatch(
          {
            d <- iteracion_emol(search_query, page = page_idx, fuentes = fuente)
            if (is.null(d) || nrow(d) == 0) {
              return(NA)
            }

            val <- NA
            if ("_source.fechaPublicacion" %in% names(d)) {
              val <- d$`_source.fechaPublicacion`[1]
            } else if ("_source.fechaModificacion" %in% names(d)) {
              val <- d$`_source.fechaModificacion`[1]
            }

            if (!is.na(val)) {
              return(lubridate::as_date(lubridate::ymd_hms(val)))
            }
            return(NA)
          },
          error = function(e) NA
        )
      }
      offset <- buscar_offset_limite(f_fin_target, total_pages, f_reciente_date, funcion_get_fecha_emol, batch_size = 1, threshold = 5)
    }
  }

  #### Iniciar el bucle repeat ####
  repeat {
    if (offset %% 10 == 0) Sys.sleep(0.2)
    datos_pagina <- tryCatch(
      {
        iteracion_emol(search_query, page = offset, fuentes = fuente)
      },
      error = function(e) {
        message("Error al obtener la pagina ", offset, " de la fuente ", fuente, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(datos_pagina) || nrow(datos_pagina) == 0) {
      message("No hay mas resultados o se produjo un error. Finalizando extraccion.")
      break
    }

    if ("_source.fechaPublicacion" %in% names(datos_pagina)) {
      datos_pagina$fecha_procesar <- lubridate::ymd_hms(datos_pagina$`_source.fechaPublicacion`)
    } else if ("_source.fechaModificacion" %in% names(datos_pagina)) {
      datos_pagina$fecha_procesar <- lubridate::ymd_hms(datos_pagina$`_source.fechaModificacion`)
    } else {
      warning("No se encontraron columnas de fecha en esta pagina. Saltando a la siguiente.")
      offset <- offset + 1
      next
    }

    if (any(!is.na(datos_pagina$fecha_procesar)) &&
      min(datos_pagina$fecha_procesar, na.rm = TRUE) < lubridate::ymd(fecha_inicio)) {
      message("Se alcanzo la fecha de inicio. Finalizando extraccion.")

      condicion_rango <- !is.na(datos_pagina$fecha_procesar) &
        datos_pagina$fecha_procesar >= lubridate::ymd(fecha_inicio) &
        datos_pagina$fecha_procesar <= lubridate::ymd(fecha_fin)

      datos_en_rango <- datos_pagina[condicion_rango, ]

      if (nrow(datos_en_rango) > 0) {
        lista_resultados[[length(lista_resultados) + 1]] <- datos_en_rango
      }

      break
    }

    condicion_fin <- !is.na(datos_pagina$fecha_procesar) &
      datos_pagina$fecha_procesar <= lubridate::ymd(fecha_fin)

    datos_en_rango <- datos_pagina[condicion_fin, ]

    if (nrow(datos_en_rango) > 0) {
      lista_resultados[[length(lista_resultados) + 1]] <- datos_en_rango
    }

    if (nrow(datos_en_rango) == 0 &&
      all(!is.na(datos_pagina$fecha_procesar)) &&
      min(datos_pagina$fecha_procesar, na.rm = TRUE) > lubridate::ymd(fecha_inicio)) {
      offset <- offset + 1
      next
    }
    offset <- offset + 1
  }

  if (length(lista_resultados) > 0) {
    all_data <- dplyr::bind_rows(lista_resultados)
  } else {
    all_data <- data.frame()
  }

  #### FIN DEL BUCLE ####

  ##############################################################################

  #### PROCESAMIENTO DF FINAL ####

  processed_data <- procesar_data_emol(all_data, fuente, search_query)

  return(processed_data)
}
