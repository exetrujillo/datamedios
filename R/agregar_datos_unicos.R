#' Agregar datos únicos a una tabla MySQL
#'
#' Esta función agrega datos a una tabla MySQL solo si los registros no están ya presentes,
#' identificando la existencia de duplicados mediante la columna `ID`.
#'
#' @param tabla Nombre de la tabla en MySQL donde se insertarán los datos.
#' @param datos Un data frame con los datos a insertar. Debe contener una columna `ID`.
#'
#' @return Mensaje indicando cuántos registros nuevos se agregaron o si no hay datos nuevos para insertar.
#'
#' @examples
#' \dontrun{
#' # Agregar datos únicos
#' agregar_datos_unicos("notas_biobio", noticias)
#' }
#'
#' @export
#'
agregar_datos_unicos <- function(tabla, datos) {
  con <- conectar_bd("credenciales.R")

  # Verificamos los IDs existentes en la tabla
  ids_existentes <- DBI::dbGetQuery(con, paste0("SELECT ID FROM ", tabla))$ID

  # Filtramos las filas del DataFrame que no están en la tabla
  datos_nuevos <- datos[!datos$ID %in% ids_existentes, ]

  # Si hay datos nuevos, los insertamos
  if (nrow(datos_nuevos) > 0) {
    DBI::dbWriteTable(con, tabla, datos_nuevos, append = TRUE, row.names = FALSE)
    cat(nrow(datos_nuevos), "nuevos registros agregados a la tabla.\n")
  } else {
    cat("No hay datos nuevos para agregar.\n")
  }
  desconectar_bd(con)
}
