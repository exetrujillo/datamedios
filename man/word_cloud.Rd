\name{word_cloud}
\alias{Nube de Palabras}
\title{Creación de nube de palabras para visualizar las noticias}
\description{
  Esta función permite realizar una nube de palabras estilizada para visualizar de forma práctica las palabras más mencionadas en el corpus de texto.
\usage{
  word_cloud(datos, max_words, stop_words = (...))
}
}
\arguments{
  \item{datos}{El data frame que otorga la función principal. Debe tener la columna \code{post_content} }

  \item{max_words}{Cuantas palabras tendrá su nube de palabras}

  \item{stop_words}{Palabras que serán ignoradas en la creación de la nube. Debe ser 1) un \code{character vector}, o 2) ES (stop_words realizada por los autores del paquete), o 3) \code{NULL}}
}
\examples{
  # Ejemplo de uso de la función
  word_cloud(datos_limpios, 60, stop_words = NULL)
}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\seealso{
  \code{\link{grafico_notas_por_mes}}: Función para visualizar que muestra la frecuencia de notas por mes en un gráfico de linea.
}
\keyword{data}
\keyword{news}
\keyword{API}
