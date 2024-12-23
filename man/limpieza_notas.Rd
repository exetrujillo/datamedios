\name{limpieza_notas}
\alias{limpieza_notas}
\title{Limpieza y filtrado de las noticias extraídas}
\description{
 Esta función permite a través de un solo código limpiar por completo el texto extraído. El input debe ser el data frame que otorga la función principal.
 Se encarga de limpiar el html, las secciones no relevantes y filtrar noticias que no contienen el término luego de la limpieza (esto ocurre ya que el
 buscador encuentra también las noticias que tienen el término en las secciones "lee también"")
}
\usage{
  limpieza_notas(datos, sinonimos = c())
}
\arguments{
  \item{datos}{Data frame que se extrae con la función principal. Debe tener la columna \code{post_content}}

  \item{sinonimos}{Para filtrar las noticias se pueden agregar sinónimos (opcional). Debe ser carácter, null o una lista vacía}
}
\examples{
  # Ejemplo de uso de la función
  datos_limpios <- limpieza_notas(raw_data, sinonimos = c("parlamento", "congreso", "diputados"))
  # El dataframe 'datos_limpios' contiene una columna ({post_content}) con el texto de las noticias limpio.
}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\keyword{data}
\keyword{news}
\keyword{API}
