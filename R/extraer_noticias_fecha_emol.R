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
#' "2025-02-24", fuente="emol")
#' }
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

  # Verificamos que el dataframe tenga todas las columnas necesarias para el nucleo
  # Las columnas para helpers seran verificadas implicitamente por los helpers
  required_columns <- c("_id", "_source.titulo", "_source.texto", "_source.permalink")
  missing_columns <- required_columns[!required_columns %in% names(all_data)]

  if (length(missing_columns) > 0) {
    message(paste("Faltan columnas base necesarias en los datos:", paste(missing_columns, collapse=", ")))
    return(create_empty_df())
  }

  all_data$search_query <- tolower(search_query)
  all_data$medio <- fuente

  fecha_source_col_name <- if (fuente == "guioteca") {
    "_source.fechaModificacion"
  } else if ("_source.fechaPublicacion" %in% names(all_data)) {
    "_source.fechaPublicacion"
  } else if ("_source.fechaModificacion" %in% names(all_data)) { # Anadido chequeo de existencia
    "_source.fechaModificacion"
  } else {
    message(paste("Advertencia: No se encontro columna de fecha '_source.fechaPublicacion' ni '_source.fechaModificacion' para fuente:", fuente, ". Las fechas seran NA."))
    NA_character_ # Para que rlang::sym no falle si no existe
  }

  processed_data <- tryCatch({
    # Antes de procesar, anadimos columnas que podrian faltar para los helpers, con NAs
    # Esto evita errores si una columna esperada por un helper no existe en all_data para alguna fuente
    cols_for_helpers <- c(
      "_source.tablas.tablaMedios" = NA, # Tipo de NA dependera, pero dplyr lo maneja
      "_source.imagen" = NA_character_,
      "_source.autor" = NA_character_,
      "_source.bajada" = NA, # Puede ser lista o df, NA es generico
      "_source.seccion" = NA_character_,
      "_source.subSeccion" = NA_character_,
      "_source.temas" = NA # Puede ser lista o df
    )

    for(col_name in names(cols_for_helpers)){
      if(!col_name %in% names(all_data)){
        all_data[[col_name]] <- cols_for_helpers[[col_name]]
      }
    }

    # Asegurar que la columna de fecha exista, aunque sea con NAs
    if (!is.na(fecha_source_col_name) && !fecha_source_col_name %in% names(all_data)) {
      all_data[[fecha_source_col_name]] <- NA_character_
    }

    all_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        ID = paste0(`_id`, "-e"),
        titulo = dplyr::coalesce(as.character(`_source.titulo`), NA_character_), # Asegurar character y manejar NULL/NA
        contenido = dplyr::coalesce(as.character(`_source.texto`), NA_character_),
        contenido_limpio = NA_character_, # Se mantiene como NA, se llenara despues
        url = dplyr::coalesce(as.character(`_source.permalink`), NA_character_),

        url_imagen = helper_extraer_url_imagen(
          `_source.tablas.tablaMedios`,
          `_source.imagen`,
          fuente # Pasamos la variable 'fuente' del entorno de la funcion principal
        ),

        autor = dplyr::coalesce(
          if (fuente == "guioteca") "guioteca" else as.character(`_source.autor`),
          NA_character_
        ),

        fecha = if (!is.na(fecha_source_col_name)) {
          as.character(as.Date(dplyr::coalesce(!!rlang::sym(fecha_source_col_name), NA_character_)))
        } else {
          NA_character_
        },

        resumen = helper_extraer_resumen(`_source.bajada`),

        # Para temas, pasamos las columnas relevantes al helper
        # El helper devolvera una lista, que es lo que se espera para una list-column
        temas = helper_extraer_temas(
          `_source.seccion`,
          `_source.subSeccion`,
          `_source.temas`,
          fuente # Pasamos la variable 'fuente'
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        ID, titulo, contenido, contenido_limpio, url,
        url_imagen, autor, fecha, temas, resumen,
        search_query, medio
      )

  }, error = function(e) {
    message(paste("--------------------------------------------------------------------"))
    message(paste("ERROR CRITICO durante el procesamiento de datos para la fuente:", fuente))
    message(paste("Mensaje de error:", e$message))

    # Intentar obtener el indice de la fila que pudo causar el error si es un error de dplyr
    # Esto es una heuristica y puede no funcionar siempre
    error_msg_str <- as.character(e)
    if (grepl("problem with `mutate()` input", error_msg_str, ignore.case = TRUE) || grepl("evaluation error", error_msg_str, ignore.case = TRUE)) {
      message("El error parece haber ocurrido durante una operacion de mutate por fila.")
      message("Es dificil identificar la fila exacta sin mas informacion del error de dplyr.")
      message("Se recomienda inspeccionar 'all_data_problematic_sample.rds' (si se crea).")
    }

    message("Estructura de 'all_data' (primeras 5 filas y columnas relevantes):")
    if (nrow(all_data) > 0) {
      cols_to_print <- intersect(c("_id", "_source.titulo", "_source.permalink", "_source.bajada", "_source.tablas.tablaMedios", "_source.imagen", "_source.seccion", "_source.subSeccion", "_source.temas", fecha_source_col_name), names(all_data))
      if (length(cols_to_print) > 0) {
        try(print(utils::head(all_data[, cols_to_print, drop = FALSE], 5)), silent = TRUE)
        message("Estructura detallada (str) de columnas potencialmente problematicas de la primera fila:")
        if ("_source.bajada" %in% names(all_data)) try(print(utils::str(all_data$`_source.bajada`[[1]])), silent = TRUE)
        if ("_source.tablas.tablaMedios" %in% names(all_data)) try(print(utils::str(all_data$`_source.tablas.tablaMedios`[[1]])), silent = TRUE)
        if ("_source.temas" %in% names(all_data)) try(print(utils::str(all_data$`_source.temas`[[1]])), silent = TRUE)
      } else {
        message("No se pudieron encontrar columnas relevantes para imprimir de 'all_data'.")
      }

      # Guardar una muestra de all_data para depuracion externa
      # Guardamos solo las primeras 100 filas o todas si son menos
      sample_size <- min(nrow(all_data), 100)
      all_data_sample <- all_data[1:sample_size, ]
      # OJO: esto guarda en el directorio de trabajo actual. Considerar una ruta mas especifica si es necesario.
      problem_file_path <- paste0("all_data_problematic_sample_FUENTE_",gsub("[^A-Za-z0-9]", "_", fuente),".rds")
      try(saveRDS(all_data_sample, file = problem_file_path), silent = TRUE)
      message(paste("Se ha guardado una muestra de datos problematicos en:", problem_file_path))
      message(paste("Puedes cargarla con: readRDS('",problem_file_path,"')",sep=""))

    } else {
      message("'all_data' esta vacio en el momento del error.")
    }
    message(paste("--------------------------------------------------------------------"))

    return(create_empty_df()) # Devuelve dataframe vacio como antes
  })

  return(processed_data)
}
