\name{extraccion_parrafos}
\alias{extraccion_parrafos}
\title{Función para extraer los párrafos por separado de cada noticia}
\description{
  Esta función genera una columna (\code{parrafos_filtrados}) en la que se almacena una lista con todos los párrafos que mencionen la search_query (con posibilidad de agregar sinónimos adicionales).
  Estos se almacenan ya limpios de todo código y secciones irrelevantes.
\usage{
  extraccion_parrafos(datos, max_words, sinonimos = c())
}
}
\arguments{
  \item{datos}{El data frame que otorga la función principal. Debe tener la columna \code{post_content} y \code{search_query}}

  \item{sinonimos}{Vector de palábras con los sinónimos deseados para la selección de párrafos. Si está vacío se utilizará solo la search_query.}
}
\examples{
  # Ejemplo de uso de la función
  extraccion_parrafos(datos, sinonimos = c("congreso", "diputados"))
}
print(datos$parafos_filtrados[[1]])

\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\seealso{
  \code{\link{limpieza_datos}}: Función para limpiar el corpus completo de noticias.
}
\keyword{data}
\keyword{news}
\keyword{API}
