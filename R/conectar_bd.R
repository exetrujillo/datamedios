#' Conectar a la base de datos
#'
#' Esta función establece una conexión con la base de datos MySQL utilizando las
#' credenciales cargadas desde un archivo R externo.
#'
#' @param archivo_credenciales Un archivo de texto R que contiene las credenciales
#' necesarias para la conexión a la base de datos. El archivo debe definir las
#' variables `db_host`, `db_port`, `db_name`, `db_user`, y `db_password`. El
#' valor por defecto es `"credenciales.R"`.
#'
#' @return Un objeto de conexión de base de datos que puede ser utilizado con las
#' funciones del paquete DBI para interactuar con la base de datos.
#'
#' @examples
#' # Conectar a la base de datos utilizando las credenciales predeterminadas
#' con <- conectar_bd("credenciales.R")
#'
#' @export
#'
conectar_bd <- function(archivo_credenciales = "credenciales.R") {
  # Verificamos que el archivo existe
  if (!file.exists(archivo_credenciales)) {
    stop("El archivo con las credenciales no se encuentra en el directorio especificado.")
  }

  # Cargamos las credenciales al entorno actual
  source(archivo_credenciales)

  # Conectamos a la base de datos usando las variables cargadas
  con <- DBI::dbConnect(
    RMySQL::MySQL(),
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password,
    dbname = db_name
  )

  return(con)
}
