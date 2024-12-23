\name{grafico_notas_por_mes}
\alias{grafico_notas_por_mes}
\title{Creación de grafico de frecuencia de notas por mes}
\description{La función permite hacer un gráfico de lineas de frecuencia de notas por mes. Se puede especificar el rango de fechas del gráfico.}
}
\usage{
  grafico_notas_por_mes(datos, fecha_inicio = "YYYY-MM-DD", fecha_fin = "YYYY-MM-DD")
}
\arguments{
  \item{datos}{Data frame extraído con la función principal. Debe contener la columna \code{raw_post_date} (obligatorio)}

  \item{fecha_inicio}{Fecha de inicio del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).}

  \item{fecha_fin}{Fecha de fin del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).}
}
\examples{
  # Ejemplo de uso de la función
  grafico_notas_por_mes(datos, titulo = "Frecuencia de Notas de fútbol por mes en 2024", fecha_inicio = "2024-01-01", fecha_fin = "2024-12-31")
}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\keyword{data}
\keyword{news}
\keyword{API}
