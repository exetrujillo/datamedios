\name{extraer_noticias_max_res}
\alias{extraer_noticias_max_res}
\title{Extracción de noticias de BíoBío con resultado máximo}
\description{
  Esta función permite realizar una extracción automatizada de noticias desde la API de BíoBío.cl, utilizando un término de búsqueda proporcionado por el usuario. El máximo son 10000 resultados.
}
\usage{
extraer_noticias_max_res(search_query, max_results = NULL)
}
\arguments{
  \item{search_query}{Una cadena de texto que especifica la frase de búsqueda (obligatoria).}
  \item{max_results}{El número máximo de resultados a extraer (opcional, por defecto todos los disponibles).}
}
\value{
  Devuelve un data frame con las noticias extraídas. El data frame contiene las siguientes columnas:
  \itemize{
    \item \code{ID}: Identificador único de la noticia.
    \item \code{post_title}: Título de la noticia.
    \item \code{post_content}: Contenido completo de la noticia.
    \item \code{post_excerpt}: Resumen o extracto de la noticia.
    \item \code{post_URL}: URL de la noticia.
    \item \code{post_categories}: Categorías asignadas a la noticia.
    \item \code{post_tags}: Etiquetas asociadas a la noticia.
    \item \code{year}, \code{month}, \code{day}: Año, mes y día de publicación.
    \item \code{post_category_primary.name}, \code{post_category_secondary.name}: Nombres de las categorías primaria y secundaria.
    \item \code{post_image.URL}: URL de la imagen asociada a la noticia.
    \item \code{post_image.alt}: Texto alternativo de la imagen asociada.
    \item \code{post_image.caption}: Leyenda de la imagen asociada.
    \item \code{author.display_name}: Nombre del autor de la noticia.
    \item \code{raw_post_date}: Fecha y hora de publicación de la noticia, en formato \code{YYYY-MM-DD}.
    \item \code{resumen_de_ia}: Resumen generado por IA de la noticia.
  }
}
\examples{
# Ejemplo de uso de la función
noticias <- extraer_noticias_max_res("inteligencia artificial", max_results = 100)
}
\seealso{
  \code{\link{init_req_bbcl}}, \code{\link{lubridate::ymd_hms}}
}
\author{
  Autores del paquete: Exequiel Trujillo e Ismael Aguayo
}
\keyword{data}
