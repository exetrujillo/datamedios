#' Procesar y estandarizar datos de BBCL (Helper)
#'
#' Esta funcion centraliza la logica de limpieza y estandarizacion de datos crudos
#' provenientes de la API de BioBioChile, convirtiendolos al formato estandar
#' del paquete datamedios.
#'
#' @param all_data Data frame con los datos crudos extraidos de la API.
#' @param search_query La frase de busqueda utilizada.
#' @return Un data frame estandarizado.
#' @export
procesar_data_bbcl <- function(all_data, search_query) {
    if (is.null(all_data) || nrow(all_data) == 0) {
        return(crear_df_vacio())
    }

    # Añadir search_query
    all_data$search_query <- tolower(search_query)

    # Formatear fecha
    if ("raw_post_date" %in% names(all_data)) {
        all_data$raw_post_date <- as.character(as.Date(all_data$raw_post_date))
    }

    # Arreglar URL de imagen
    if ("post_image.URL" %in% names(all_data)) {
        all_data$post_image.URL <- paste0("https://media.biobiochile.cl/wp-content/uploads/", as.character(all_data$post_image.URL))
    }

    # Creacion de la columna "temas" (lista)
    # Usamos mapply/lapply vectorizado donde sea posible, pero al ser estructura anidada (listas de dataframes),
    # un lapply simple sobre las filas suele ser seguro.

    temas_list <- lapply(seq_len(nrow(all_data)), function(i) {
        slugs_categorias <- character()
        if ("post_categories" %in% names(all_data) && !is.null(all_data$post_categories[[i]])) {
            if (is.data.frame(all_data$post_categories[[i]])) {
                slugs_categorias <- all_data$post_categories[[i]]$slug
            }
        }

        slugs_tags <- character()
        if ("post_tags" %in% names(all_data) && !is.null(all_data$post_tags[[i]])) {
            if (is.data.frame(all_data$post_tags[[i]])) {
                slugs_tags <- all_data$post_tags[[i]]$slug
            }
        }

        # Combinar ambos y devolver
        c(slugs_categorias, slugs_tags)
    })

    all_data$temas <- temas_list

    # Eliminar columnas originales complejas
    all_data$post_categories <- NULL
    all_data$post_tags <- NULL

    # Definir columna medio
    all_data$medio <- "bbcl"

    # Renombrar columnas al estandar
    # Usamos dplyr::rename si existe la columna, o mutate si necesitamos computar
    # Mapeo:
    # post_title -> titulo
    # post_content -> contenido
    # post_URL -> url
    # author.display_name -> autor
    # raw_post_date -> fecha
    # resumen_de_ia -> resumen
    # post_content_clean -> contenido_limpio
    # post_image.URL -> url_imagen

    all_data <- all_data %>%
        dplyr::rename_with(
            .fn = function(x) {
                dplyr::recode(x,
                    post_title = "titulo",
                    post_content = "contenido",
                    post_URL = "url",
                    `author.display_name` = "autor",
                    raw_post_date = "fecha",
                    resumen_de_ia = "resumen",
                    post_content_clean = "contenido_limpio",
                    `post_image.URL` = "url_imagen"
                )
            }
        )

    # Asegurar existencia de todas las columnas estandar
    cols_standard <- names(crear_df_vacio())

    for (col in cols_standard) {
        if (!col %in% names(all_data)) {
            if (col == "temas") {
                all_data[[col]] <- vector("list", nrow(all_data))
            } else {
                all_data[[col]] <- NA_character_
            }
        }
    }

    # Seleccionar y ordenar final
    all_data <- all_data[, cols_standard, drop = FALSE]

    return(all_data)
}

#' Obtener fecha de la primera noticia de un lote (BBCL Helper)
#'
#' Helper para la navegacion rapida (busqueda binaria).
#'
#' @param off Offset de paginacion.
#' @param search_query Termino de busqueda.
#' @param headers Headers HTTP para la solicitud.
#' @return Fecha de la primera noticia encontrada o NA.
#' @export
obtener_fecha_offset_bbcl <- function(off, search_query, headers = NULL) {
    if (is.null(headers)) {
        headers <- c(
            `User-Agent` = get_random_user_agent(),
            `Accept` = "application/json, text/plain, */*",
            `Referer` = paste0("https://www.biobiochile.cl/buscador.shtml?s=", utils::URLencode(search_query)),
            `Content-Type` = "application/json; charset=UTF-8"
        )
    }

    u <- paste0(
        "https://www.biobiochile.cl/lista/api/buscador?offset=", off,
        "&search=", utils::URLencode(search_query),
        "&intervalo=&orden=ultimas"
    )
    tryCatch(
        {
            resp <- httr::GET(u, httr::add_headers(.headers = headers))
            if (resp$status_code == 200) {
                d <- httr::content(resp, "text", encoding = "UTF-8") %>%
                    jsonlite::fromJSON(flatten = TRUE)
                if (!is.null(d$notas) && length(d$notas$raw_post_date) > 0) {
                    return(as.Date(d$notas$raw_post_date[1])) # Retornar la primera fecha del lote
                }
            }
            return(NA)
        },
        error = function(e) NA
    )
}
