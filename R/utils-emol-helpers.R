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
    if (fuente_actual %in% c("emol", "mediosregionales")) {
      if (is.data.frame(source_tablas_medios) && nrow(source_tablas_medios) > 0 && "Url" %in% names(source_tablas_medios)) {
        url_img <- as.character(source_tablas_medios[1, "Url"])
      }
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

    # Limpieza final: asegurar que no sea una lista, dataframe, o vector multiple
    if (is.list(url_img) || is.data.frame(url_img) || length(url_img) == 0) {
      url_img <- NA_character_
    } else if (length(url_img) > 1) {
      url_img <- url_img[1] # Tomar solo el primero si es un vector
    }
    if (is.character(url_img) && nzchar(url_img) == FALSE) url_img <- NA_character_ # Convertir "" a NA

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
#' @return Un string (character) con el texto del resumen, o `NA_character_`
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
      } else if (!is.list(source_temas) && !is.data.frame(source_temas)) { # Si es un vector atomico
        t_vector <- anadir_termino(t_vector, source_temas) # Esto podria anadir multiples si source_temas es un vector
        # Si source_temas es un vector, y anadir_termino solo toma el primer elemento implicitamente,
        # se podria necesitar iterar: for(st in source_temas) { t_vector <- anadir_termino(t_vector, st) }
        # Pero anadir_termino ya hace c(vec, as.character(termino)), lo cual maneja vectores.
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
