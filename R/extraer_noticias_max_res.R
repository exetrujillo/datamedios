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
extraer_noticias_max_res <- function(search_query = NULL, max_results = NULL, subir_a_bd = TRUE, fuentes = "todas") {
  # Validamos los parametros
  if (is.null(search_query) && fuentes != "ciper") {
    stop("Debe proporcionar una frase de busqueda si la fuente no es exclusivamente 'ciper'.")
  }
  if (!is.null(search_query) && !is.character(search_query)) {
    stop("La frase de busqueda debe ser texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un numero entero positivo o NULL.")
  }

  ##############################################################################

  # Inicializamos variables y objetos
  patronFuentes = ""
  all_data <- data.frame(
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

  # Indice de fuentes
  if(fuentes=="todas"){
    patronFuentes <- "bbcl, emol-todas, ciper"
  } else {
    patronFuentes <- fuentes
  }

  fuentesParseadas <- parserFuentes(patronFuentes)
  message(paste0("Fuentes parseadas: ", fuentesParseadas))

  ##############################################################################
  # CONJUNTOS DE FUENTES

  # emol
  fuentes_emol <- c("emol", "mediosregionales", "guioteca")

  ##############################################################################
  # SELECTOR DE EJECUCIONES

  #### BBCL ####
  if ("bbcl" %in% fuentesParseadas) {
    # Ejecutar la funcion para bbcl
    data_bbcl <- extraer_noticias_max_res_bbcl(search_query, max_results = max_results)
    if (!is.null(data_bbcl) && nrow(data_bbcl) > 0) {
      all_data <- rbind(all_data, data_bbcl)
    }
  }

  #### EMOL ####
  if ("emol-todas" %in% fuentesParseadas) {
    # Ejecutar la funcion para cada fuente de emol por separado
    for (fuente in fuentes_emol) {
      data_emol <- extraer_noticias_max_res_emol(search_query, max_results = max_results, fuente = fuente)
      if (!is.null(data_emol) && nrow(data_emol) > 0) {
        all_data <- rbind(all_data, data_emol)
      }
    }
  } else {
    # Filtrar las fuentes de emol seleccionadas
    fuentes_emol_seleccionadas <- intersect(fuentesParseadas, fuentes_emol)
    if (length(fuentes_emol_seleccionadas) > 0) {
      # Ejecutar la funcion para cada fuente seleccionada individualmente
      for (fuente in fuentes_emol_seleccionadas) {
        data_emol <- extraer_noticias_max_res_emol(search_query, max_results = max_results, fuente = fuente)
        if (!is.null(data_emol) && nrow(data_emol) > 0) {
          all_data <- rbind(all_data, data_emol)
        }
      }
    }
  }

  #### Ciper ####
  if ("ciper" %in% fuentesParseadas) {
    # Ejecutar la funcion para ciper
    data_ciper <- extraer_noticias_max_res_ciper(search_query, max_results = max_results)
    if (!is.null(data_ciper) && nrow(data_ciper) > 0) {
      all_data <- rbind(all_data, data_ciper)
    }
  }

  ##############################################################################


  ##############################################################################

  # Subimos a la base de datos en caso de que el parametro subir_a_db es TRUE
  if (subir_a_bd) {
    tryCatch({
      # Llamamos a la funcion que sube los datos si subir_a_bd es TRUE
      agregar_datos_unicos(all_data)
    }, error = function(e) {
      message("Ocurrio un error al intentar agregar los datos a la base de datos: ", e$message)
    })
  }

  message(paste0("Noticias hasta la fecha: ", nrow(all_data)))

  return(all_data)
}
