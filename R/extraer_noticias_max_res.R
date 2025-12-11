#' Extraccion de noticias de medios chilenos por cantidad maxima de resultados
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias
#' de BioBio y fuentes de El Mercurio.
#'
#' Es importante mencionar que si tiene mas de una fuente seleccionada, la
#' cantidad maxima de resultados se aplicara para cada una de las fuentes, es
#' decir, si pones max_results = 10 y tienes fuentes = "emol,guioteca,bbcl"
#' tendras como maximo 30 resultados.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param max_results Numero maximo de resultados a extraer (opcional, por defecto todos).
#' @param subir_a_bd por defecto TRUE, FALSE para test y cosas por el estilo (opcional).
#' @param fuentes por defecto marca todas las fuentes, pero se puede elegir una o varias de las disponibles en el README. (opcional)
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_max_res("inteligencia artificial",
#' max_results = 20, fuentes="bbcl, emol", subir_a_bd = FALSE)
#' }
#' @export
extraer_noticias_max_res <- function(search_query, max_results = NULL, subir_a_bd = TRUE, fuentes = "todas") {
  # Validamos los parametros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda valida como texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un numero entero positivo o NULL.")
  }

  ##############################################################################

  # Inicializamos variables y objetos
  patronFuentes = ""
  
  # Estructura vacia por defecto
  # Estructura vacia por defecto
  empty_df <- crear_df_vacio()
  
  lista_resultados <- list()

  # Unificar input de fuentes si viene como vector
  if (length(fuentes) > 1) {
    patronFuentes <- paste(fuentes, collapse = ", ")
  } else {
    patronFuentes <- fuentes
  }

  # Indice de fuentes (expansion de macro "todas")
  if(patronFuentes == "todas"){
    patronFuentes <- "bbcl, emol-todas, ciper"
  }

  fuentesParseadas <- parserFuentes(patronFuentes)
  message(paste0("Fuentes parseadas: ", fuentesParseadas))

  ##############################################################################
  # CONJUNTOS DE FUENTES

  # emol
  fuentes_emol <- get_fuentes_emol()

  ##############################################################################
  # SELECTOR DE EJECUCIONES

  #### BBCL ####
  if ("bbcl" %in% fuentesParseadas) {
    tryCatch({
      # Ejecutar la funcion para bbcl
      data_bbcl <- extraer_noticias_max_res_bbcl(search_query, max_results = max_results)
      if (!is.null(data_bbcl) && nrow(data_bbcl) > 0) {
        lista_resultados[[length(lista_resultados) + 1]] <- data_bbcl
      }
    }, error = function(e) {
      message("Error extrayendo noticias de BioBio: ", e$message)
    })
  }

  #### EMOL ####
  if ("emol-todas" %in% fuentesParseadas) {
    # Ejecutar la funcion para cada fuente de emol por separado
    for (fuente in fuentes_emol) {
      tryCatch({
        data_emol <- extraer_noticias_max_res_emol(search_query, max_results = max_results, fuente = fuente)
        if (!is.null(data_emol) && nrow(data_emol) > 0) {
          lista_resultados[[length(lista_resultados) + 1]] <- data_emol
        }
      }, error = function(e) {
        message(paste0("Error extrayendo noticias de ", fuente, ": ", e$message))
      })
    }
  } else {
    # Filtrar las fuentes de emol seleccionadas
    fuentes_emol_seleccionadas <- intersect(fuentesParseadas, fuentes_emol)
    if (length(fuentes_emol_seleccionadas) > 0) {
      # Ejecutar la funcion para cada fuente seleccionada individualmente
      for (fuente in fuentes_emol_seleccionadas) {
        tryCatch({
          data_emol <- extraer_noticias_max_res_emol(search_query, max_results = max_results, fuente = fuente)
          if (!is.null(data_emol) && nrow(data_emol) > 0) {
            lista_resultados[[length(lista_resultados) + 1]] <- data_emol
          }
        }, error = function(e) {
           message(paste0("Error extrayendo noticias de ", fuente, ": ", e$message))
        })
      }
    }
  }

  #### Ciper ####
  if ("ciper" %in% fuentesParseadas) {
    tryCatch({
      # Ejecutar la funcion para ciper
      data_ciper <- extraer_noticias_max_res_ciper(search_query, max_results = max_results)
      if (!is.null(data_ciper) && nrow(data_ciper) > 0) {
        lista_resultados[[length(lista_resultados) + 1]] <- data_ciper
      }
    }, error = function(e) {
      message("Error extrayendo noticias de Ciper: ", e$message)
    })
  }

  ##############################################################################


  ##############################################################################
  
  # Unir todos los resultados
  if (length(lista_resultados) > 0) {
    all_data <- dplyr::bind_rows(lista_resultados)
  } else {
    all_data <- empty_df
  }

  # Subimos a la base de datos en caso de que el parametro subir_a_db es TRUE
  if (subir_a_bd) {
    tryCatch({
      # Llamamos a la funcion que sube los datos si subir_a_bd es TRUE
      agregar_datos_unicos(all_data)
    }, error = function(e) {
      message("Ocurrio un error al intentar agregar los datos a la base de datos: ", e$message)
    })
  }

  message(paste0("Noticias hasta la fecha: ", all_data$fecha[nrow(all_data)]))

  return(all_data)
}
