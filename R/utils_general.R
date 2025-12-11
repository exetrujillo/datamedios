#' Crear Dataframe Vacio Estandarizado
#'
#' Crea un dataframe vacio con la estructura estandar del paquete datamedios.
#' Asegura consistencia en las columnas y tipos de datos entre todos los scrapers.
#'
#' @return Un data frame con 0 filas y las columnas estandar.
#' @keywords internal
crear_df_vacio <- function() {
  df <- data.frame(
    ID = character(),
    titulo = character(),
    contenido = character(),
    contenido_limpio = character(),
    url = character(),
    url_imagen = character(),
    autor = character(),
    fecha = character(),
    resumen = character(),
    search_query = character(),
    medio = character(),
    stringsAsFactors = FALSE
  )
  # Asignar columna de lista vacia explicitamente para asegurar su creacion
  df$temas <- list()
  return(df)
}

#' Obtener User-Agent Aleatorio
#'
#' Devuelve una cadena de User-Agent seleccionada aleatoriamente de una lista predefinida.
#' Util para rotar identificadores y evitar bloqueos simples en scrapers.
#'
#' @return Cadena de caracteres con un User-Agent.
#' @keywords internal
get_random_user_agent <- function() {
  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:121.0) Gecko/20100101 Firefox/121.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.2 Safari/605.1.15",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36 Edg/119.0.0.0"
  )
  sample(user_agents, 1)
}

#' Buscar Offset Limite (Busqueda Binaria)
#'
#' Encuentra el offset aproximado donde comienzan a aparecer las noticias correspondientes a `fecha_objetivo`.
#' Dado que las APIs suelen entregar las noticias ordenadas de mas reciente (offset 0) a mas antigua,
#' esta funcion busca el punto de quiebre donde las noticias dejan de ser "futuras" y entran en el rango de fechas deseado.
#'
#' @param fecha_objetivo Fecha (Date) mas reciente del rango que queremos extraer (fecha_fin).
#' @param total_results Numero total de resultados disponibles.
#' @param fecha_reciente Fecha de la noticia mas reciente (offset 0).
#' @param batch_size Tamano del lote o pagina (por defecto 20).
#' @param threshold Umbral de convergencia para detener la busqueda binaria (por defecto 500).
#' @return Un integer con el offset estimado para comenzar la busqueda fina.
#' @keywords internal
buscar_offset_limite <- function(fecha_objetivo, total_results, fecha_reciente, funcion_get_fecha, batch_size = 20, threshold = 500) {
  # Si la fecha reciente ya es mas antigua que la objetivo, empezamos desde 0
  if (fecha_reciente <= fecha_objetivo) {
    message("  Fecha reciente (", fecha_reciente, ") ya es <= objetivo (", fecha_objetivo, "). Offset 0.")
    return(0)
  }

  # Obtener fecha antigua (extremo final)
  # Probamos con el ultimo offset posible (total - batch, o ajustado a pagina)
  offset_final <- max(0, total_results - batch_size)
  fecha_final_raw <- funcion_get_fecha(offset_final)

  if (is.null(fecha_final_raw) || is.na(fecha_final_raw)) {
    message("  No se pudo obtener fecha del final. Usando busqueda secuencial.")
    return(0)
  }
  fecha_final <- as.Date(fecha_final_raw)

  # Si la fecha final es mas reciente que la objetivo (sitio raro o pocas noticias), scan total
  if (fecha_final > fecha_objetivo) {
    message("  Incluso la ultima noticia es mas reciente que el objetivo. Revisar todo.")
    return(0)
  }

  # Variables para busqueda binaria
  low_idx <- 0
  high_idx <- offset_final
  low_date <- as.numeric(fecha_reciente) # Mas reciente es Mayor numero (hoy > ayer)
  high_date <- as.numeric(fecha_final) # Mas antigua es Menor numero
  target_date_num <- as.numeric(fecha_objetivo)

  # Iterar para acercarse
  max_iter <- 20
  iter_count <- 0

  while ((high_idx - low_idx) > threshold && iter_count < max_iter) {
    iter_count <- iter_count + 1

    if (low_date == high_date) break

    # Punto medio (Binary Search)
    mid_idx <- floor((low_idx + high_idx) / 2)

    # Ajustar a multiplo de batch_size (paginado)
    if (batch_size > 1) {
      mid_idx <- floor(mid_idx / batch_size) * batch_size
    }

    # Evitar repeticion de extremos (avance minimo)
    if (mid_idx == low_idx) mid_idx <- low_idx + batch_size
    if (mid_idx == high_idx) mid_idx <- high_idx - batch_size

    if (mid_idx >= high_idx || mid_idx <= low_idx) {
      break
    }
    
    # Descomentar para debug
    # message(paste("  [Binaria] Iter:", iter_count, "| Rango:", low_idx, "-", high_idx, "| Mid:", mid_idx))

    # Checkear fecha en punto medio
    check_date <- funcion_get_fecha(mid_idx)

    if (is.null(check_date) || is.na(check_date)) {
      message("  [Binaria] Fallo al obtener fecha en offset ", mid_idx, ".")
      break
    }

    check_date_num <- as.numeric(check_date)

    if (check_date_num > target_date_num) {
      # Fecha encontrada > objetivo (Mas reciente). Objetivo esta mas al pasado (Offset Mayor).
      # Ajustamos LOW al punto medio
      low_idx <- mid_idx
      low_date <- check_date_num
    } else {
      # Fecha encontrada <= objetivo (Mas antigua o igual). Objetivo esta mas al presente (Offset Menor).
      # Ajustamos HIGH al punto medio
      high_idx <- mid_idx
      high_date <- check_date_num
    }
  }

  return(low_idx)
}

#' Obtener Fuentes de Emol
#'
#' Devuelve un vector con los nombres de las fuentes disponibles en el grupo Emol.
#' Util para iterar sobre todas las fuentes cuando se solicita 'emol-todas'.
#'
#' @return Vector de caracteres con las fuentes.
#' @keywords internal
get_fuentes_emol <- function() {
  c("emol", "mediosregionales", "guioteca")
}
