\name{tabla_frecuencia_palabras}
\alias{tabla_frecuencia_palabras}
\title{Tabla de palabras más frecuentes en el corpus de texto}
\description{
  Esta función permite realizar una tabla con las palabras más mencionadas en las noticias extraídas.
}
\usage{
  tabla_frecuencia_palabras(datos, max_words = numeric, stop_words = c())
}
\arguments{
  \item{datos}{Data frame que se extrae con la función principal, y pasó por el procedimiento de limpieza (ver {\link{limpieza_datos}}). Debe contener la columna \code{post_content}}

  \item{max_words}{Cantidad de filas que tendrá la tabla}

  \item{stop_words}{Vector de carácteres que definirá las palabras ignoradas para crear la tabla}
}
\value{
  Una tabla con las n (max_words) palabras más frecuentes en el corpus de texto.
  }
}
\examples{
  # Ejemplo de uso de la función
  top_words <- tabla_frecuencia_palabras(datos_limpios, max_words = 5, stop_words = c("el", "de"))
  top_words

}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\seealso{
  \code{\link{word_cloud}}: Función que realiza una nube de palabras con las palabras más frecuentes.
}
\keyword{data}
\keyword{news}
\keyword{API}
