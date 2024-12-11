\name{conectar_bd}
\alias{conectar_bd}
\title{Conectar a la base de datos MySQL}
\description{
  Esta función establece una conexión con la base de datos MySQL utilizando las credenciales cargadas desde un archivo R externo.
}
\usage{
conectar_bd(archivo_credenciales = "credenciales.R")
}
\arguments{
  \item{archivo_credenciales}{Un archivo de texto R que contiene las credenciales necesarias para la conexión a la base de datos. El archivo debe definir las variables \code{db_host}, \code{db_port}, \code{db_name}, \code{db_user}, y \code{db_password}. El valor por defecto es \code{"credenciales.R"}.}
}
\value{
  Retorna un objeto de conexión de base de datos que puede ser utilizado con las funciones del paquete DBI para interactuar con la base de datos.
}
\examples{
# Conectar a la base de datos utilizando las credenciales predeterminadas
con <- conectar_bd("credenciales.R")
}
\seealso{
  \code{\link[DBI]{dbConnect}} para más detalles sobre la conexión a bases de datos.
}
