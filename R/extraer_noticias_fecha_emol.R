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
#' noticias <- extraer_noticias_fecha_emol("inteligencia artificial", "2025-01-01",
#' "2025-02-24", fuente="emol")
#' @export

extraer_noticias_fecha_emol <- function(search_query, fecha_inicio, fecha_fin, fuente) {
  #### Funcion auxiliar para crear dataframe vacio con estructura correcta ####
  create_empty_df <- function() {
    data.frame(
      ID = character(),
      titulo = character(),
      contenido = character(),
      contenido_limpio = character(),
      url = character(),
      url_imagen = character(),
      autor = character(),
      fecha = character(),
      temas = list(),
      resumen = character(),
      search_query = character(),
      medio = character(),
      stringsAsFactors = FALSE
    )
  }

  #### Inicializamos variables ####
  offset <- 0
  total_results <- 0
  fecha_mas_reciente <- NA

  # Dataframe para recopilar todos los resultados
  all_data <- data.frame()
  ##############################################################################

  #### Obtenemos la respuesta inicial ####
  respuesta_inicial <- tryCatch({
    init_req_emol(search_query, fuentes = fuente)
  }, error = function(e) {
    message("Error al obtener la respuesta inicial: ", e$message)
    return(NULL)
  })

  # Verificamos si hay resultados en la respuesta inicial
  if (is.null(respuesta_inicial) || nrow(respuesta_inicial) == 0) {
    message(paste("No se encontraron resultados iniciales para la fuente:", fuente))
    return(create_empty_df())
  }

  if ("_source.fechaPublicacion" %in% names(respuesta_inicial)) {
    fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$`_source.fechaPublicacion`[1])
  } else if ("_source.fechaModificacion" %in% names(respuesta_inicial)) {
    fecha_mas_reciente <- lubridate::ymd_hms(respuesta_inicial$`_source.fechaModificacion`[1])
  } else {
    message("Advertencia: Ninguna de las columnas '_source.fechaPublicacion' o '_source.fechaModificacion' existe en la respuesta inicial.")
    return(create_empty_df())
  }
  message(paste("Fecha mas reciente:", fecha_mas_reciente))

  ##############################################################################

  #### Iniciar el bucle repeat ####
  repeat {
    # Obtener los datos de la pagina actual
    datos_pagina <- tryCatch({
      iteracion_emol(search_query, page = offset, fuentes = fuente)
    }, error = function(e) {
      message("Error al obtener la pagina ", offset, " de la fuente ", fuente,": ", e$message)
      return(NULL)
    })

    # Verificar si hay datos
    if (is.null(datos_pagina) || nrow(datos_pagina) == 0) {
      message("No hay mas resultados o se produjo un error. Finalizando extraccion.")
      break
    }

    # Procesar las fechas en los datos obtenidos
    if ("_source.fechaPublicacion" %in% names(datos_pagina)) {
      datos_pagina$fecha_procesar <- lubridate::ymd_hms(datos_pagina$`_source.fechaPublicacion`)
    } else if ("_source.fechaModificacion" %in% names(datos_pagina)) {
      datos_pagina$fecha_procesar <- lubridate::ymd_hms(datos_pagina$`_source.fechaModificacion`)
    } else {
      warning("No se encontraron columnas de fecha en esta pagina. Saltando a la siguiente.")
      offset <- offset + 1
      next
    }

    # Verificar si ya pasamos la fecha de inicio (demasiado antiguo)
    if (any(!is.na(datos_pagina$fecha_procesar)) &&
        min(datos_pagina$fecha_procesar, na.rm = TRUE) < lubridate::ymd(fecha_inicio)) {
      message("Se alcanzo la fecha de inicio. Finalizando extraccion.")

      # Filtrar solo los registros dentro del rango de fechas
      datos_en_rango <- datos_pagina[datos_pagina$fecha_procesar >= lubridate::ymd(fecha_inicio) &
                                       datos_pagina$fecha_procesar <= lubridate::ymd(fecha_fin), ]

      if (nrow(datos_en_rango) > 0) {
        # Agregar directamente los datos al dataframe final sin procesamiento
        all_data <- rbind(all_data, datos_en_rango)
      }

      break
    }

    # Filtrar los registros que estan dentro del rango de fechas
    datos_en_rango <- datos_pagina[datos_pagina$fecha_procesar <= lubridate::ymd(fecha_fin), ]

    if (nrow(datos_en_rango) > 0) {
      # Agregar directamente los datos al dataframe final sin procesamiento
      all_data <- rbind(all_data, datos_en_rango)
    }

    # Si no hay datos en rango pero aun no hemos llegado a la fecha de inicio, continuamos
    if (nrow(datos_en_rango) == 0 &&
        all(!is.na(datos_pagina$fecha_procesar)) &&
        min(datos_pagina$fecha_procesar, na.rm = TRUE) > lubridate::ymd(fecha_inicio)) {
      offset <- offset + 1
      next
    }

    # Incrementamos el offset para la siguiente iteracion
    offset <- offset + 1
  }

  #### FIN DEL BUCLE ####

  ##############################################################################

  #### PROCESAMIENTO DF FINAL ####

  # Verificamos si hay datos para procesar
  if (is.null(all_data) || nrow(all_data) == 0) {
    message(paste("No se encontraron datos para la fuente:", fuente, "en el rango de fechas especificado."))
    return(create_empty_df())
  }

  # Verificamos que el dataframe tenga todas las columnas necesarias
  required_columns <- c("_id", "_source.titulo", "_source.texto", "_source.permalink")
  missing_columns <- required_columns[!required_columns %in% names(all_data)]

  if (length(missing_columns) > 0) {
    message(paste("Faltan columnas necesarias en los datos:", paste(missing_columns, collapse=", ")))
    return(create_empty_df())
  }

  # Agregamos columnas adicionales de metadatos
  all_data$search_query <- tolower(search_query)
  all_data$medio <- fuente

  # Determinar la columna de fecha a utilizar
  fecha_source <- if (fuente == "guioteca") {
    "_source.fechaModificacion"
  } else if ("_source.fechaPublicacion" %in% names(all_data)) {
    "_source.fechaPublicacion"
  } else {
    "_source.fechaModificacion"
  }

  # Procesar los datos utilizando el enfoque rowwise
  processed_data <- tryCatch({
    all_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ID = paste0(`_id`, "-e"),
        titulo = `_source.titulo`,
        contenido = `_source.texto`,
        contenido_limpio = NA,
        url = `_source.permalink`,
        url_imagen = if (fuente %in% c("emol", "mediosregionales")) {
          x <- `_source.tablas.tablaMedios`
          if (is.data.frame(x) && "Url" %in% names(x)) as.character(x[1, "Url"]) else NA
        } else {
          `_source.imagen`
        },
        autor = if (fuente == "guioteca") "guioteca" else `_source.autor`,
        fecha = as.character(as.Date(dplyr::coalesce(!!rlang::sym(fecha_source)))),
        # Extraer el resumen como un valor escalar
        resumen = {
          bajada <- `_source.bajada`
          if (is.null(bajada)) {
            NA_character_
          } else if (is.data.frame(bajada)) {
            as.character(bajada[1,1])
          } else if (is.list(bajada)) {
            if (length(bajada) > 0) as.character(bajada[[1]]) else NA_character_
          } else {
            as.character(bajada)
          }
        }
      ) %>%
      dplyr::mutate(
        temas = list({
          if (fuente == "mediosregionales") {
            # Si la fuente es "mediosregionales", usar seccion y subSeccion
            t <- c(`_source.seccion`, `_source.subSeccion`)
          } else {
            # Caso normal: usar seccion y temas
            t <- `_source.seccion`
            if ("_source.temas" %in% colnames(all_data)) {
              aux <- `_source.temas`
              if (!is.null(aux) && length(aux) > 0 && "nombre" %in% names(aux)) {
                t <- c(t, as.character(aux$nombre))
              }
            }
          }
          t <- tolower(t)
          t <- gsub("\\s+", "-", t)
          t
        })
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID, titulo, contenido, contenido_limpio, url, url_imagen, autor, fecha, temas, resumen, search_query, medio)
  }, error = function(e) {
    message("Error al procesar los datos: ", e$message)
    return(create_empty_df())
  })

  return(processed_data)
}
