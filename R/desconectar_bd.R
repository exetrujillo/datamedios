#' Desconectar la base de datos
#'
#' Esta función cierra la conexión activa a la base de datos.
#'
#' @param con Un objeto de conexión creado por la función `conectar_bd()`.
#'
#' @return No retorna nada. Solo cierra la conexión.
#'
#' @examples
#' con <- conectar_bd("credenciales.R")
#' desconectar_bd(con)
#'
#' @export
desconectar_bd <- function(con) {
  # Verifica si la conexión es válida antes de intentar cerrarla
  if (!inherits(con, "MySQLConnection")) {
    stop("El objeto no es una conexión válida.")
  }

  # Cierra la conexión
  DBI::dbDisconnect(con)
}
