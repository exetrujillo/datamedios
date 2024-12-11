\name{desconectar_bd}
\alias{desconectar_bd}
\title{Desconectar de la base de datos MySQL}
\description{
  Esta función cierra la conexión activa a la base de datos.
}
\usage{
desconectar_bd(con)
}
\arguments{
  \item{con}{Un objeto de conexión creado por la función \code{conectar_bd()}.}
}
\value{
  No retorna nada. Solo cierra la conexión.
}
\examples{
# Establecer la conexión
con <- conectar_bd("credenciales.R")

# Realizar operaciones con la base de datos...

# Desconectar después de terminar
desconectar_bd(con)
}
\seealso{
  \code{\link[DBI]{dbDisconnect}} para más detalles sobre el cierre de conexiones.
}
