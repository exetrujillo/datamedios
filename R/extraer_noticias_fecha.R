#' Extraccion de noticias de medios chilenos por rango de fechas
#'
#' Esta funcion permite realizar una extraccion automatizada de noticias de
#' BioBio o Los medios de Emol, utilizando un rango de fechas.
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param fecha_inicio Fecha de inicio del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param fecha_fin Fecha de fin del rango de busqueda en formato "YYYY-MM-DD" (obligatoria).
#' @param subir_a_bd por defecto TRUE, FALSE para test y cosas por el estilo (opcional).
#' @param fuentes es un string con las fuentes a extraer. Puede ser bbcl o las de emol.
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' noticias <- extraer_noticias_fecha("inteligencia artificial", "2025-03-25",
#' "2025-04-24", subir_a_bd = FALSE, fuentes="bbcl")
#' @export

extraer_noticias_fecha <- function(search_query, fecha_inicio, fecha_fin, subir_a_bd = TRUE, fuentes="todas") {
  # Validamos los parametros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda como texto.")
  }
  if (missing(fecha_inicio) || missing(fecha_fin) || !lubridate::is.Date(lubridate::ymd(fecha_inicio)) || !lubridate::is.Date(lubridate::ymd(fecha_fin))) {
    stop("Debe proporcionar fechas de inicio y fin validas en formato 'YYYY-MM-DD'.")
  }
  if (lubridate::ymd(fecha_inicio) > lubridate::ymd(fecha_fin)) {
    stop("La fecha de inicio debe ser anterior o igual a la fecha de fin.")
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
    patronFuentes <- "bbcl, emol-todas"
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
    data_bbcl <- extraer_noticias_fecha_bbcl(search_query, fecha_inicio = fecha_inicio, fecha_fin = fecha_fin)
    if (!is.null(data_bbcl) && nrow(data_bbcl) > 0) {
      all_data <- rbind(all_data, data_bbcl)
    }
  }

  #### EMOL ####
  if ("emol-todas" %in% fuentesParseadas) {
    # Ejecutar la funcion para cada fuente de emol por separado
    for (fuente in fuentes_emol) {
      data_emol <- extraer_noticias_fecha_emol(search_query, fecha_inicio, fecha_fin, fuente = fuente)
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
        data_emol <- extraer_noticias_fecha_emol(search_query, fecha_inicio, fecha_fin, fuente = fuente)
        if (!is.null(data_emol) && nrow(data_emol) > 0) {
          all_data <- rbind(all_data, data_emol)
        }
      }
    }
  }

  #### ACA IREMOS AGREGANDO MAS FUENTES CON SUS FUNCIONES ESPECIFICAS ####

  ##############################################################################

  # Subimos a la base de datos en caso de que el parametro subir_a_db es TRUE
  if (subir_a_bd && nrow(all_data) > 0) {
    tryCatch({
      # Llamamos a la funcion que sube los datos si subir_a_bd es TRUE
      agregar_datos_unicos(all_data)
    }, error = function(e) {
      message("Ocurrio un error al intentar agregar los datos a la base de datos: ", e$message)
    })
  }

  ##############################################################################

  return(all_data)
}
