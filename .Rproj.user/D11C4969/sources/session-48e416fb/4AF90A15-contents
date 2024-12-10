\name{extraer_noticias}
\alias{extraer_noticias}
\title{Extracción de noticias desde la API de BíoBío.cl}
\description{
  Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl. Los usuarios pueden especificar una frase de búsqueda y un número máximo de resultados a extraer.
}
\usage{
  extraer_noticias(search_query, max_results = NULL)
}
\arguments{
  \item{search_query}{Una frase de búsqueda que será utilizada para consultar las noticias. Este parámetro es obligatorio y debe ser un texto (carácter).}
  \item{max_results}{Número máximo de resultados a extraer. Es un parámetro opcional; por defecto se extraen todos los resultados disponibles.}
}
\value{
  Un dataframe con las noticias extraídas que incluye diversas columnas como título, contenido, URL, categorías, etiquetas, año, mes, autor, imagen, etc.
}
\details{
  La función realiza una consulta a la API de BíoBío.cl, extrayendo información sobre las noticias que coincidan con la frase de búsqueda proporcionada. Si se especifica un número máximo de resultados, la función limita la extracción de noticias. Los resultados incluyen información detallada sobre cada noticia, como su título, resumen, categorías, etiquetas, autor, fecha de publicación y otros metadatos.
}
\examples{
  # Ejemplo de uso de la función
  noticias <- extraer_noticias("inteligencia artificial", max_results = 100)
}
\author{
  Tu nombre o los colaboradores del paquete
}
\seealso{
  \code{\link{httr}}, \code{\link{jsonlite}}, \code{\link{dplyr}}
}
\keyword{data}
