\name{agregar_datos_unicos}
\alias{agregar_datos_unicos}
\title{Agregar datos únicos a una tabla MySQL}
\description{
Esta función agrega datos a una tabla MySQL solo si los registros no están ya presentes, identificando la existencia de duplicados mediante la columna \code{ID}. Además, esta función depende del archivo \code{credenciales.R}, donde se deben especificar las credenciales para conectarse a una base de datos MySQL.
}
\usage{
agregar_datos_unicos(tabla, datos)
}
\arguments{
  \item{tabla}{Nombre de la tabla en MySQL donde se insertarán los datos.}
  \item{datos}{Un data frame con los datos a insertar. Debe contener una columna \code{ID}.}
}
\value{
Mensaje indicando cuántos registros nuevos se agregaron o si no hay datos nuevos para insertar.
}
\examples{
\dontrun{
# Agregar datos únicos
agregar_datos_unicos("notas_biobio", noticias)
}
}
\seealso{
\code{\link[DBI]{dbGetQuery}}, \code{\link[DBI]{dbWriteTable}}
}
\author{Autores del paquete: Exequiel Trujillo e Ismael Aguayo}
\keyword{MySQL, datos, tabla}
