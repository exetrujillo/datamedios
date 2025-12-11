#' Extraccion de noticias de Emol.com
#'
#' Esta funcion permite extraer noticias de las fuentes de Emol, tanto de las
#' noticias no pagas de emol, como de guioteca y los medios regionales de El Mercurio
#'
#' @param search_query Una frase de busqueda (obligatoria).
#' @param max_results Numero maximo de resultados a extraer (opcional, por defecto todos).
#' @param fuente Fuente de emol para iterar (obligatoria).
#' @return Un dataframe con las noticias extraidas.
#' @examples
#' \dontrun{
#' noticias <- extraer_noticias_max_res_emol("inteligencia artificial", "2025-01-01",
#' "2025-02-24", fuente="mediosregionales")
#' }
#' @export
extraer_noticias_max_res_emol <- function(search_query, max_results=NULL, fuente){
  # Validamos los parametros
  if (missing(search_query) || !is.character(search_query)) {
    stop("Debe proporcionar una frase de busqueda valida como texto.")
  }
  if (!is.null(max_results) && (!is.numeric(max_results) || max_results <= 0)) {
    stop("max_results debe ser un numero entero positivo o NULL.")
  }

  # Variables iniciales
  
  # Logica recursiva para 'emol-todas'
  if (fuente == "emol-todas") {
    fuentes_loop <- get_fuentes_emol()
    lista_res <- list()
    
    for (f in fuentes_loop) {
      tryCatch({
        message(paste("Iniciando extraccion recursiva para fuente:", f))
        df_temp <- extraer_noticias_max_res_emol(search_query, max_results, fuente = f)
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

  total_results <- 0

  # Dataframe para recopilar todos los resultados
  all_data <- crear_df_vacio()

  # Obtener la respuesta inicial
  respuesta_inicial <- tryCatch({
    init_req_emol(search_query, fuentes = fuente)
  }, error = function(e) {
    message("Error al obtener la respuesta inicial: ", e$message)
    return(NULL)
  })

  # Verificar si hay resultados en la respuesta inicial
  if (is.null(respuesta_inicial) || nrow(respuesta_inicial) == 0) {
    message(paste("No se encontraron resultados iniciales para la fuente:", fuente))
    return(create_empty_df())
  }

  # Calcular total de resultados
  total_results <- tryCatch({
    as.numeric(respuesta_inicial$total[1])
  }, error = function(e) {
    message("No se pudo convertir total de resultados: ", e$message)
    NA_real_
  })

  # Verificar total de resultados
  if (is.na(total_results) || total_results <= 0) {
    message("No se pudo determinar el numero total de resultados.")
    return(create_empty_df())
  }

  # Determinar numero maximo de resultados a extraer
  if (is.null(max_results) || max_results > total_results) {
    max_results <- total_results
  }

  # Inicializar variables para el bucle
  lista_resultados <- list()
  count_results <- 0
  all_data <- create_empty_df()
  offset <- 0

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

    # Agregar los datos a la lista
    lista_resultados[[length(lista_resultados) + 1]] <- datos_pagina
    count_results <- count_results + nrow(datos_pagina)

    # Verificar si alcanzamos el numero maximo de resultados
    if (count_results >= max_results) {
      break
    }

    # Incrementar el offset para la siguiente iteracion
    offset <- offset + 1

    # Opcional: agregar un retraso para no sobrecargar la API
    # Sys.sleep(0.5)
  }

  # Combinar resultados
  if (length(lista_resultados) > 0) {
    all_data <- dplyr::bind_rows(lista_resultados)
    if (nrow(all_data) > max_results) {
       all_data <- all_data[1:max_results, ]
    }
  }

  ##################################################################################

  #### PROCESAMIENTO DF FINAL ####

  processed_data <- procesar_data_emol(all_data, fuente, search_query)

  return(processed_data)
}
