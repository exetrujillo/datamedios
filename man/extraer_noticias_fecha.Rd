\name{extraer_noticias_fecha}
\alias{extraer_noticias_fecha}
\title{Extracción de noticias desde la API de BíoBío.cl por rango de fechas}
\description{
  Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl utilizando un rango de fechas.
  Las noticias extraídas corresponden a un término de búsqueda específico y son filtradas por las fechas de publicación.
}
\usage{
  extraer_noticias_fecha(search_query, fecha_inicio, fecha_fin)
}
\arguments{
  \item{search_query}{Una frase de búsqueda que se usará para filtrar las noticias (obligatorio). Debe ser una cadena de caracteres.}

  \item{fecha_inicio}{Fecha de inicio del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).}

  \item{fecha_fin}{Fecha de fin del rango de búsqueda en formato "YYYY-MM-DD" (obligatorio).}
}
\value{
  Un dataframe con las noticias extraídas, que incluye:
  \itemize{
    \item{ID}{: ID de la noticia.}
    \item{post_title}{: Título de la noticia.}
    \item{post_content}{: Contenido completo de la noticia.}
    \item{post_excerpt}{: Resumen o extracto de la noticia.}
    \item{post_URL}{: Enlace a la noticia en el sitio web.}
    \item{post_categories}{: Categorías a las que pertenece la noticia.}
    \item{post_tags}{: Etiquetas asociadas a la noticia.}
    \item{year, month, day}{: Fecha de la noticia en formato año, mes y día.}
    \item{post_category_primary.name}{: Categoría principal de la noticia.}
    \item{post_category_secondary.name}{: Categoría secundaria de la noticia.}
    \item{post_image.URL}{: URL de la imagen asociada a la noticia.}
    \item{post_image.alt}{: Texto alternativo de la imagen.}
    \item{post_image.caption}{: Leyenda de la imagen.}
    \item{author.display_name}{: Nombre del autor de la noticia.}
    \item{raw_post_date}{: Fecha de publicación de la noticia.}
    \item{resumen_de_ia}{: Resumen generado por inteligencia artificial, si está disponible.}
  }
}
\examples{
  # Ejemplo de uso de la función
  noticias <- extraer_noticias_fecha("inteligencia artificial", "2023-01-01", "2023-12-31")
  # El dataframe 'noticias' contiene las noticias dentro del rango de fechas especificado
}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\seealso{
  \code{\link{extraer_noticias}}: Función que extrae noticias sin restricciones de fechas, sólo con un máximo de resultados (opcional).
}
\keyword{data}
\keyword{news}
\keyword{API}
