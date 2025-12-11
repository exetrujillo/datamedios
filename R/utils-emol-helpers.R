# R/utils-emol-helpers.R

#' Helper para extraer la URL de la imagen de datos de Emol
#'
#' Esta funcion interna procesa las columnas relevantes de una fila de datos
#' de Emol para extraer la URL de la imagen principal de la noticia.
#' Prioriza `_source.tablas.tablaMedios` para fuentes "emol" y "mediosregionales",
#' y recurre a `_source.imagen` para otras o si la primera falla.
#' Maneja diferentes estructuras de datos (dataframes, listas, valores atomicos).
#'
#' @param source_tablas_medios El contenido de la columna `_source.tablas.tablaMedios`
#'   de la fila actual. Puede ser un dataframe, lista, o NULL.
#' @param source_imagen El contenido de la columna `_source.imagen` de la fila actual.
#'   Puede ser un string, lista, o NULL.
#' @param fuente_actual Un string que indica la fuente de la noticia (e.g., "emol",
#'   "guioteca", "mediosregionales").
#'
#' @return Un string (character) con la URL de la imagen, o `NA_character_` si
#'   no se puede extraer una URL valida o si ocurre un error.
#' @keywords internal
helper_extraer_url_imagen <- function(source_tablas_medios, source_imagen, fuente_actual) {
  url_img <- NA_character_
  tryCatch({
    # Intentar extraer de tablaMedios (Comun en Emol y MediosRegionales)
    if (is.data.frame(source_tablas_medios) && nrow(source_tablas_medios) > 0 && "Url" %in% names(source_tablas_medios)) {
      url_img <- as.character(source_tablas_medios[1, "Url"])
    }

    # Fallback o para otras fuentes como guioteca
    if (is.na(url_img) || !(fuente_actual %in% c("emol", "mediosregionales"))) {
      if (!is.null(source_imagen) && !is.list(source_imagen) && !is.data.frame(source_imagen) && !is.na(source_imagen)) {
        url_img <- as.character(source_imagen)
      } else if (is.list(source_imagen) && length(source_imagen) > 0) {
        # Si es una lista, tomar el primer elemento si no es otra lista/df
        primer_elem <- source_imagen[[1]]
        if(!is.null(primer_elem) && !is.na(primer_elem) && !is.list(primer_elem) && !is.data.frame(primer_elem)){
          url_img <- as.character(primer_elem)
        }
      }
    }

    if (is.list(url_img) || is.data.frame(url_img) || length(url_img) == 0) {
      url_img <- NA_character_
    } else if (length(url_img) > 1) {
      url_img <- url_img[1]
    }
    if (is.character(url_img) && nzchar(url_img) == FALSE) url_img <- NA_character_

  }, error = function(e) {
    url_img <- NA_character_
  })
  return(url_img)
}


#' Helper para extraer el resumen (bajada) de datos de Emol
#'
#' Esta funcion interna procesa la columna `_source.bajada` de una fila
#' de datos de Emol para extraer el texto del resumen.
#' Maneja casos donde `_source.bajada` puede ser NULL, un dataframe,
#' una lista, o un valor atomico.
#'
#' @param bajada_val El contenido de la columna `_source.bajada` de la fila actual.
#'
#' @return Un string con el texto del resumen, o `NA_character_`
#'   si no se puede extraer o si ocurre un error.
#' @keywords internal
helper_extraer_resumen <- function(bajada_val) {
  resultado_resumen <- NA_character_
  tryCatch({
    if (!is.null(bajada_val)) {
      if (is.data.frame(bajada_val)) {
        if (nrow(bajada_val) > 0 && ncol(bajada_val) > 0) {
          val_celda <- bajada_val[1,1]
          if (!is.null(val_celda) && !is.na(val_celda) && !is.list(val_celda) && !is.data.frame(val_celda)) {
            resultado_resumen <- as.character(val_celda)
          }
        }
      } else if (is.list(bajada_val)) {
        if (length(bajada_val) > 0) {
          primer_elemento_lista <- bajada_val[[1]]
          if (is.data.frame(primer_elemento_lista)) {
            if (nrow(primer_elemento_lista) > 0 && ncol(primer_elemento_lista) > 0) {
              val_celda_lista <- primer_elemento_lista[1,1]
              if(!is.null(val_celda_lista) && !is.na(val_celda_lista) && !is.list(val_celda_lista) && !is.data.frame(val_celda_lista)){
                resultado_resumen <- as.character(val_celda_lista)
              }
            }
          } else if (!is.list(primer_elemento_lista)) { # Evitar listas anidadas de forma recursiva simple
            if(!is.null(primer_elemento_lista) && !is.na(primer_elemento_lista)){
              resultado_resumen <- as.character(primer_elemento_lista)
            }
          }
        }
      } else if (!is.na(bajada_val)) { # Valor atomico
        resultado_resumen <- as.character(bajada_val)
      }
    }

    # Limpieza final
    if (is.list(resultado_resumen) || is.data.frame(resultado_resumen) || length(resultado_resumen) == 0) {
      resultado_resumen <- NA_character_
    } else if (length(resultado_resumen) > 1) {
      resultado_resumen <- resultado_resumen[1]
    }
    if (is.character(resultado_resumen) && nzchar(resultado_resumen) == FALSE) resultado_resumen <- NA_character_

  }, error = function(e) {
    resultado_resumen <- NA_character_
  })
  return(resultado_resumen)
}


#' Helper para extraer y procesar los temas de datos de Emol
#'
#' Esta funcion interna procesa las columnas `_source.seccion`,
#' `_source.subSeccion` (si aplica), y `_source.temas` de una fila de
#' datos de Emol para generar una lista de temas procesados.
#' Los temas se convierten a minusculas y los espacios se reemplazan por guiones.
#'
#' @param source_seccion El contenido de la columna `_source.seccion`.
#' @param source_subseccion El contenido de la columna `_source.subSeccion`.
#' @param source_temas El contenido de la columna `_source.temas`. Puede ser
#'   un dataframe, lista, vector, o NULL.
#' @param fuente_actual Un string que indica la fuente (e.g., "mediosregionales").
#'
#' @return Una lista que contiene un unico elemento: un vector de strings (character)
#'   con los temas procesados y unicos. Si no hay temas, devuelve una lista
#'   conteniendo un vector de caracteres vacio (`character(0)`).
#' @keywords internal
helper_extraer_temas <- function(source_seccion, source_subseccion, source_temas, fuente_actual) {
  temas_lista_final <- list(character(0)) # Valor por defecto
  tryCatch({
    t_vector <- c()

    # Funcion interna para anadir terminos validos al vector de temas
    anadir_termino <- function(vec, termino) {
      if (!is.null(termino) && !is.na(termino) && !is.list(termino) && !is.data.frame(termino) && nzchar(as.character(termino))) {
        vec <- c(vec, as.character(termino))
      }
      return(vec)
    }

    if (fuente_actual == "mediosregionales") {
      t_vector <- anadir_termino(t_vector, source_seccion)
      t_vector <- anadir_termino(t_vector, source_subseccion)
    } else {
      t_vector <- anadir_termino(t_vector, source_seccion)
    }

    # Manejo de source_temas
    if (!is.null(source_temas)) {
      if (is.data.frame(source_temas) && nrow(source_temas) > 0 && "nombre" %in% names(source_temas)) {
        nombres_temas <- source_temas$nombre
        for(nombre_tema in nombres_temas) {
          t_vector <- anadir_termino(t_vector, nombre_tema)
        }
      } else if (is.list(source_temas) && length(source_temas) > 0) {
        elementos_temas_procesados <- unlist(lapply(source_temas, function(x_tema) {
          if(!is.list(x_tema) && !is.data.frame(x_tema) && !is.null(x_tema) && !is.na(x_tema) && nzchar(as.character(x_tema))) {
            as.character(x_tema)
          } else {
            NULL # Omitir elementos no validos o complejos
          }
        }))
        t_vector <- c(t_vector, elementos_temas_procesados)
      } else if (!is.list(source_temas) && !is.data.frame(source_temas)) {
        t_vector <- anadir_termino(t_vector, source_temas)
      }
    }

    if (length(t_vector) > 0) {
      t_vector_limpio <- tolower(t_vector[!is.na(t_vector) & t_vector != "" & sapply(t_vector, nzchar)])
      t_vector_limpio <- gsub("\\s+", "-", t_vector_limpio)
      temas_lista_final <- list(unique(t_vector_limpio[nzchar(t_vector_limpio)]))
    }
    # Si temas_lista_final sigue siendo list(character(0)) despues de esto, esta bien.

  }, error = function(e) {
    # En caso de error, devuelve una lista con un vector de caracteres vacio
    temas_lista_final <- list(character(0))
  })
  return(temas_lista_final)
}

#' Procesar y estandarizar datos crudos de Emol
#'
#' Funcion auxiliar centralizada para limpiar, rellenar y estandarizar los datos
#' provenientes de las iteraciones de Emol (API). Realiza la conversion de listas
#' a dataframes planos con las 12 columnas estandar.
#' Utiliza `mapply` para optimizar el rendimiento.
#'
#' @param all_data Dataframe crudo acumulado de la extraccion.
#' @param fuente String con el nombre de la fuente (e.g. "emol", "guioteca").
#' @param search_query String con la consulta de busqueda realizada.
#'
#' @return Dataframe final estandarizado.
#' @keywords internal
procesar_data_emol <- function(all_data, fuente, search_query) {
  # Validacion inicial
  if (is.null(all_data) || nrow(all_data) == 0) {
    message(paste("No se encontraron datos para la fuente:", fuente, "en el rango especificado."))
    return(crear_df_vacio())
  }

  # Validacion de columnas base requeridas
  required_columns <- c("_id", "_source.titulo", "_source.texto", "_source.permalink")
  missing_columns <- required_columns[!required_columns %in% names(all_data)]

  if (length(missing_columns) > 0) {
    message(paste("Faltan columnas base necesarias en los datos:", paste(missing_columns, collapse = ", ")))
    return(crear_df_vacio())
  }

  all_data$search_query <- tolower(search_query)

  # Determinacion de columna de fecha
  fecha_source_col_name <- if (fuente == "guioteca") {
    "_source.fechaModificacion"
  } else if ("_source.fechaPublicacion" %in% names(all_data)) {
    "_source.fechaPublicacion"
  } else if ("_source.fechaModificacion" %in% names(all_data)) {
    "_source.fechaModificacion"
  } else {
    message(paste("Advertencia: No se encontro columna de fecha '_source.fechaPublicacion' ni '_source.fechaModificacion' para fuente:", fuente, ". Las fechas seran NA."))
    NA_character_
  }

  processed_data <- tryCatch({
    # Rellenar columnas faltantes para helpers
    cols_for_helpers <- c(
      "_source.tablas.tablaMedios" = NA,
      "_source.imagen" = NA_character_,
      "_source.autor" = NA_character_,
      "_source.bajada" = NA,
      "_source.seccion" = NA_character_,
      "_source.subSeccion" = NA_character_,
      "_source.temas" = NA
    )

    for (col_name in names(cols_for_helpers)) {
      if (!col_name %in% names(all_data)) {
        all_data[[col_name]] <- cols_for_helpers[[col_name]]
      }
    }

    # Asegurar columna fecha
    if (!is.na(fecha_source_col_name) && !fecha_source_col_name %in% names(all_data)) {
      all_data[[fecha_source_col_name]] <- NA_character_
    }
    
    # Pre-calculo de vectores optimizados con mapply
    
    # 1. URL Imagen
    vec_url_imagen <- mapply(
      helper_extraer_url_imagen,
      all_data[["_source.tablas.tablaMedios"]],
      all_data[["_source.imagen"]],
      MoreArgs = list(fuente_actual = fuente),
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
    
    # 2. Resumen
    vec_resumen <- mapply(
      helper_extraer_resumen,
      all_data[["_source.bajada"]],
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
    
    # 3. Temas (Devuelve list of lists, queremos list of vectors)
    list_temas_raw <- mapply(
      helper_extraer_temas,
      all_data[["_source.seccion"]],
      all_data[["_source.subSeccion"]],
      all_data[["_source.temas"]],
      MoreArgs = list(fuente_actual = fuente),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
    list_temas <- lapply(list_temas_raw, function(x) if(is.list(x)) x[[1]] else character(0))

    # Mutate vectorizado
    all_data %>%
      dplyr::mutate(
        ID = paste0(`_id`, "-e"),
        titulo = dplyr::coalesce(as.character(`_source.titulo`), NA_character_),
        contenido = dplyr::coalesce(as.character(`_source.texto`), NA_character_),
        contenido_limpio = NA_character_,
        url = dplyr::coalesce(as.character(`_source.permalink`), NA_character_),
        
        # Asignamos los vectores pre-calculados
        url_imagen = vec_url_imagen,
        
        autor = dplyr::coalesce(
          if (fuente == "guioteca") "guioteca" else as.character(`_source.autor`),
          NA_character_
        ),
        
        fecha = if (!is.na(fecha_source_col_name)) {
          as.character(as.Date(dplyr::coalesce(!!rlang::sym(fecha_source_col_name), NA_character_)))
        } else {
          NA_character_
        },
        
        resumen = vec_resumen,
        temas = list_temas,
        
        search_query = search_query,
        medio = fuente
      ) %>%
      dplyr::select(
        ID, titulo, contenido, contenido_limpio, url,
        url_imagen, autor, fecha, temas, resumen,
        search_query, medio
      )

  }, error = function(e) {
    message(paste("--------------------------------------------------------------------"))
    message(paste("ERROR CRITICO durante el procesamiento de datos (centralizado) para:", fuente))
    message(paste("Mensaje:", e$message))
    
    # Log simple de error
    error_msg_str <- as.character(e)
    if (grepl("problem", error_msg_str, ignore.case = TRUE)) {
      message("Posible error en transformacion de columnas.")
    }

    # Guardado de muestra debug
    if (nrow(all_data) > 0) {
      sample_size <- min(nrow(all_data), 100)
      all_data_sample <- all_data[1:sample_size, ]
      problem_file_path <- paste0("debug_data_FAIL_", gsub("[^A-Za-z0-9]", "_", fuente), ".rds")
      try(saveRDS(all_data_sample, file = problem_file_path), silent = TRUE)
      message(paste("Muestra guardada en:", problem_file_path))
    }
    
    message(paste("--------------------------------------------------------------------"))
    return(crear_df_vacio())
  })
  
  # Final sanity check de columnas
  cols_standard <- names(crear_df_vacio())
  process_cols <- names(processed_data)
  
  # Rellenar con NAs si falta algo (aunque el select deberia manejarlo o fallar)
  for (col in cols_standard) {
    if (!col %in% process_cols) {
       if (col == "temas") {
        processed_data[[col]] <- vector("list", nrow(processed_data))
      } else {
        processed_data[[col]] <- NA_character_
      }
    }
  }
  
  return(processed_data[, cols_standard, drop = FALSE])
}
